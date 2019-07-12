open Lwt.Infix

module Job = Current.Job

let timestamp = ref Unix.gettimeofday
let sleep = ref Lwt_unix.sleep

let confirm (confirmed, level) =
  match Lwt.state confirmed with
  | Lwt.Return () -> Lwt.return_unit
  | _ ->
    Log.info (fun f -> f "Waiting for confirm-level >= %a" Current.Level.pp level);
    confirmed >|= fun () ->
    Log.info (fun f -> f "Confirm-level now >= %a" Current.Level.pp level)

module Schedule = struct
  type t = {
    valid_for : Duration.t option;
  }

  let v ?valid_for () = { valid_for }

  let default = v ()
end

let durations = Duration.[
  to_year, "years";
  to_day, "days";
  to_hour, "hours";
  to_min, "minutes";
  to_sec, "seconds"
]

let pp_duration_rough f d =
  let rec aux = function
    | [] -> Fmt.pf f "%.3f s" (Duration.to_f d)
    | (to_int, units) :: xs ->
      let i = to_int d in
      if i < 5 then aux xs
      else Fmt.pf f "%d %s" i units
  in
  aux durations

module Make(B : S.BUILDER) = struct
  module Builds = Map.Make(String)

  type build_result = B.Value.t Current.or_error * float  (* The final result and end time *)

  type build = {
    job_id : Current.Input.job_id;
    key : B.Key.t;
    build_number : int64;           (* Increments on rebuild *)
    switch : Current.Switch.t;      (* Turning this off aborts the build. *)
    finished : build_result Lwt.t;  (* Unresolved if the build is still in progress. *)
    mutable auto_cancelled : bool;  (* Don't record the result (because the build was auto-cancelled). *)
    mutable ref_count : int;        (* The number of watchers waiting for the result (for auto-cancel). *)
  }

  let builds : build Builds.t ref = ref Builds.empty

  let is_running build = Lwt.state build.finished = Lwt.Sleep

  let force_invalidate key =
    builds := Builds.remove key !builds;
    Db.Build.invalidate ~builder:B.id key

  (* Mark this build as invalid. The caller is responsible for triggering a re-evaluation. *)
  let invalidate key =
    let key_digest = B.Key.digest key in
    match Builds.find_opt key_digest !builds with
    | Some build when is_running build -> Fmt.failwith "invalidate(%a): build is still running!" B.pp key
    | _ -> force_invalidate key_digest

  (* This thread runs in the background to perform the build.
     When done, it records the result in the database (unless it was auto-cancelled). *)
  let do_build ~config ~job ctx build =
    let ready = !timestamp () |> Unix.gmtime in
    let running = ref None in
    let is_finished = ref false in
    Job.log job "Starting build for %a" B.pp build.key;
    let level = B.level ctx build.key in
    confirm (Current.Config.confirmed level config, level) >>= fun () ->
    running := Some (!timestamp () |> Unix.gmtime);
    Lwt.catch
      (fun () -> B.build ~switch:build.switch ctx job build.key)
      (fun ex -> Lwt.return @@ Error (`Msg (Printexc.to_string ex)))
    >|= fun x ->
    is_finished := true;
    let finished = !timestamp () in
    if build.auto_cancelled then (
      force_invalidate (B.Key.digest build.key);
      (* If anyone started wanting this build again after it got cancelled,
         then they will now recalculate, discover the build doesn't exist, and
         trigger a new one. *)
      Error (`Msg "Auto-cancelled"), finished
    ) else (
      let record result =
        Db.Build.record
          ~builder:B.id
          ~build:build.build_number
          ~key:(B.Key.digest build.key)
          ~job:(Job.id job)
          ~ready ~running:!running ~finished:(Unix.gmtime finished)
          result;
        x, finished
      in
      match x with
      | Ok v ->
        Job.log job "Success";
        record @@ Ok (B.Value.marshal v)
      | Error (`Msg m) as e ->
        Job.log job "Failed: %s" m;
        record e
    )

  (* Create a new in-memory build object. Try to initialise it with the value from the disk-cache.
     If there isn't one, start a new build in a background thread. *)
  let get_build ~config ctx key =
    let key_digest = B.Key.digest key in
    let finished, set_finished = Lwt.wait () in
    let previous = Db.Build.lookup ~builder:B.id key_digest in
    match previous with
    | Some { Db.Build.rebuild = false; build = build_number; value; finished = fin_time; job_id } ->
      Log.info (fun f -> f "@[<hov2>Loaded cached result for@ %a@]" B.pp key);
      let v =
        match value with
        | Ok v -> Ok (B.Value.unmarshal v)
        | Error _ as e -> e
      in
      Lwt.wakeup set_finished (v, fin_time);
      let switch = Current.Switch.create_off @@ Error (`Msg "Loaded from cache") in
      { job_id; switch; build_number; key; finished; ref_count = 0; auto_cancelled = false }
    | _ ->
      let build_number =
        match previous with
        | None -> 0L
        | Some x -> Int64.succ x.Db.Build.build       (* A rebuild was requested *)
      in
      let switch = Current.Switch.create ~label:(Fmt.strf "Build %a" B.pp key) () in
      let job = Job.create ~switch ~label:B.id () in
      let build = { job_id = Job.id job; switch; build_number; key; finished; ref_count = 0; auto_cancelled = false } in
      Lwt.async (fun () ->
          Lwt.try_bind
            (fun () ->
               Lwt.finalize
                 (fun () -> do_build ~config ~job ctx build)
                 (fun () -> Current.Switch.turn_off build.switch @@ Ok ())
            )
            (fun result -> Lwt.wakeup set_finished result; Lwt.return_unit)
            (fun ex -> Lwt.wakeup_exn set_finished ex; Lwt.return_unit)
        );
      build

  (* The build must be currently running when calling this. *)
  let input_running build =
    build.ref_count <- build.ref_count + 1;
    let cancel ~msg () =
      Lwt.async (fun () -> Current.Switch.turn_off build.switch @@ Error (`Msg msg))
    in
    let watch =
      object
        method pp f = B.pp f build.key
        method changed = Lwt.map ignore build.finished
        method cancel = Some (cancel ~msg:"Cancelled by user")
        method release =
          build.ref_count <- build.ref_count - 1;
          if build.ref_count = 0 && B.auto_cancel && is_running build then (
            build.auto_cancelled <- true;
            cancel ~msg:"Auto-cancelling job because it is no longer needed" ()
          )
      end
    in
    Error `Pending, Some build.job_id, [watch]

  (* The build must be finished when calling this. *)
  let input_finished ~schedule build (value, finished) =
    match schedule.Schedule.valid_for with
    | None -> (value :> B.Value.t Current_term.Output.t), Some build.job_id, []
    | Some duration ->
      let remaining_time = Duration.to_f duration +. finished -. !timestamp () in
      let changed =
        if remaining_time <= 0.0 then (
          Log.info (fun f -> f "Build result for %a has expired" B.pp build.key);
          invalidate build.key;
          Lwt.return_unit       (* Trigger an immediate recalculation *)
        ) else (
          !sleep remaining_time
        )
      in
      let watch =
        object
          method pp f =
            if remaining_time <= 0.0 then
              Fmt.pf f "%a (expired)" B.pp build.key
            else
              Fmt.pf f "%a will be invalid after %a"
                B.pp build.key
                pp_duration_rough (Duration.of_f remaining_time)

          method changed = changed
          method cancel = None
          method release = ()
        end
      in
      (value :> B.Value.t Current_term.Output.t), Some build.job_id, [watch]

  let input build ~schedule =
    match Lwt.state build.finished with
    | Lwt.Sleep -> input_running build
    | Lwt.Return v -> input_finished build ~schedule v
    | Lwt.Fail ex -> raise ex

  let get ?(schedule=Schedule.default) ctx key =
    Current.Input.of_fn @@ fun config ->
    let key_digest = B.Key.digest key in
    let b =
      match Builds.find_opt key_digest !builds with
      | Some b -> b
      | None ->
        let b = get_build ~config ctx key in
        builds := Builds.add key_digest b !builds;
        b
    in
    input b ~schedule

  let reset () =
    builds := Builds.empty;
    Db.Build.drop_all B.id
end

module Output(Op : S.PUBLISHER) = struct
  module Outputs = Map.Make(String)

  module Value : sig
    (** Cache the value of the digest. *)

    type t

    val v : Op.Value.t -> t
    val value : t -> Op.Value.t
    val digest : t -> string
    val equal : t -> t -> bool
  end = struct
    type t = {
      value : Op.Value.t;
      digest : string;
    }

    let v value = { value; digest = Op.Value.digest value }
    let value t = t.value
    let digest t = t.digest
    let equal a b = a.digest = b.digest
  end

  (* An entry in the database means we think the output has been successfully set to that value. *)

  type op = {
    value : Value.t;                (* The value currently being set. *)
    switch : Current.Switch.t;      (* Turning this off aborts the operation. *)
    finished : unit Lwt.t;
    mutable autocancelled : bool;   (* This op is expected to fail *)
  }

  type output = {
    key : Op.Key.t;
    mutable job_id : Current.Input.job_id option; (* Current or last log *)
    mutable current : string option;      (* The current digest value, if known. *)
    mutable desired : Value.t;            (* The value we want. *)
    mutable ctx : Op.t;                   (* The context for [desired]. *)
    mutable op : [
      | `Running of op                    (* The currently-running operation. *)
      | `Error of [`Msg of string]        (* Why [desired] isn't possible. *)
      | `Finished
    ];
  }

  let pp_op f (k, v) = Op.pp f (k, Value.value v)
  let pp_desired f output = pp_op f (output.key, output.desired)

  let outputs : output Outputs.t ref = ref Outputs.empty

  let pp_output f output =
    match output.op with
    | `Error (`Msg msg) ->
      Fmt.pf f "%a: %s" pp_desired output msg
    | `Finished ->
      Fmt.pf f "%a (completed)" pp_desired output
    | `Running op ->
      if Value.equal op.value output.desired then
        Fmt.pf f "%a (in-progress)" pp_op (output.key, op.value)
      else
        Fmt.pf f "%a (stale), then %a"
          pp_op (output.key, op.value)
          pp_desired output

  (* If output isn't in (or moving to) the desired state, start a thread to do that,
     unless we already tried that and failed. *)
  let rec maybe_start ~config output =
    match output.op with
    | `Error _ -> () (* Wait for error to be cleared. *)
    | `Running _ when not Op.auto_cancel ->
      (* Already publishing something and we don't auto-cancel.
         When the stale push completes, we'll get called again. *)
      ()
    | `Running op when Value.equal output.desired op.value ->
      (* We're already working to set the desired value. Just keep going. *)
      ()
    | `Running op ->
      assert Op.auto_cancel;
      Log.info (fun f -> f "Auto-cancelling %a" pp_op (output.key, op.value));
      op.autocancelled <- true;
      (* Cancel existing job. When that finishes, we'll get called again. *)
      Lwt.async (fun () ->
          Current.Switch.turn_off op.switch @@
          Error (`Msg "Auto-cancelling job because it is no longer needed")
        );
    | `Finished ->
      match output.current with
      | Some current when current = Value.digest output.desired -> () (* Already the desired value. *)
      | _ ->
        (* Either we don't know the current state, or we know we want something different.
           We're not already running, and we haven't already failed. Time to publish! *)
        (* Once we start publishing, we don't know the state: *)
        output.current <- None;
        Db.Publish.invalidate ~op:Op.id (Op.Key.digest output.key);
        output.op <- `Running (publish ~config output)
  and publish ~config output =
    let finished, set_finished = Lwt.wait () in
    let ctx = output.ctx in
    let switch = Current.Switch.create ~label:Op.id () in
    let op = { value = output.desired; switch; finished; autocancelled = false } in
    Lwt.async
      (fun () ->
         Lwt.finalize
           (fun () ->
              let pp_op f = pp_op f (output.key, op.value) in
              let job = Job.create ~switch ~label:Op.id () in
              output.job_id <- Some (Job.id job);
              Job.log job "Publish: %t" pp_op;
              Lwt.catch
                (fun () ->
                   let level = Op.level ctx output.key (Value.value op.value) in
                   confirm (Current.Config.confirmed level config, level) >>= fun () ->
                   Op.publish ~switch ctx job output.key (Value.value op.value)
                )
                (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))
              >|= function
              | Ok () ->
                Job.log job "Publish(%t) : succeeded" pp_op;
                output.current <- Some (Value.digest op.value);
                output.op <- `Finished;
                let job_id = Job.id job in
                Db.Publish.record ~op:Op.id ~job_id ~key:(Op.Key.digest output.key) (Value.digest op.value);
                (* While we were pushing, we might have decided we wanted something else.
                   If so, start pushing that now. *)
                maybe_start ~config output;
                Lwt.wakeup set_finished ()
              | Error (`Msg m as e) ->
                if op.autocancelled then
                  Job.log job "Auto-cancel(%t) complete (%s)" pp_op m
                else
                  Job.log job "Publish(%t) failed: %s" pp_op m;
                let retry =
                  (* If it failed because we cancelled it, don't count that as an error. *)
                  op.autocancelled ||
                  (* If it failed but we have a new value to set, ignore the stale error. *)
                  not (Value.equal op.value output.desired)
                in
                output.op <- if retry then `Finished else `Error e;
                maybe_start ~config output;
                Lwt.wakeup set_finished ()
           )
           (fun () ->
              Current.Switch.turn_off switch @@ Ok ()
           )
      );
    op

  (* Create a new in-memory [op], initialising it from the database. *)
  let get_op ctx key desired =
    let current, job_id =
      match Db.Publish.lookup ~op:Op.id (Op.Key.digest key) with
      | Some { Db.Publish.value; job_id } -> Some value, Some job_id
      | None -> None, None
    in
    { key; current; desired; ctx; op = `Finished; job_id }

  let set ctx key value =
    Current.Input.of_fn @@ fun config ->
    Log.debug (fun f -> f "set: %a" Op.pp (key, value));
    let key_digest = Op.Key.digest key in
    let value = Value.v value in
    let o =
      (* Ensure the [op] exists and has [op.desired = value]: *)
      match Outputs.find_opt key_digest !outputs with
      | Some o ->
        o.ctx <- ctx;
        if not (Value.equal value o.desired) then (
          o.desired <- value;
          match o.op with
          | `Error _ -> o.op <- `Finished   (* Clear error when the desired value changes. *)
          | `Running _ | `Finished -> ()
        );
        o
      | None ->
        let o = get_op ctx key value in
        outputs := Outputs.add key_digest o !outputs;
        o
    in
    maybe_start ~config o;
    match o.op with
    | `Finished -> Ok (), o.job_id, []
    | `Error e -> (Error e :> unit Current_term.Output.t), o.job_id, []
    | `Running op ->
      let watch =
        object
          method pp f = pp_output f o
          method changed = op.finished
          method cancel = None
          method release = ()
        end
      in
      Error `Pending, o.job_id, [watch]

  let reset () =
    outputs := Outputs.empty;
    Db.Publish.drop_all Op.id
end

module S = S
