open Lwt.Infix

module Job = Current.Job

let rebuild_cond = Lwt_condition.create ()
(* This triggers when the user invalidates an entry and we should re-evaluate.
   Ideally this condition would be per-job, but since we currently re-evaluate
   everything anyway, a global is fine. *)

let confirm ~job (confirmed, level) =
  match Lwt.state confirmed with
  | Lwt.Return () -> Lwt.return_unit
  | _ ->
    Job.log job "Waiting for confirm-threshold > %a" Current.Level.pp level;
    Log.info (fun f -> f "Waiting for confirm-threshold > %a" Current.Level.pp level);
    confirmed >|= fun () ->
    Job.log job "Confirm-threshold now > %a" Current.Level.pp level;
    Log.info (fun f -> f "Confirm-threshold now > %a" Current.Level.pp level)

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

  type build_result = {
    job_id : string;
    value : B.Value.t Current.or_error;
    end_time : float;
  }

  type build = {
    mutable job : [
      | `Active of Current.Job.t * build_result Lwt.t
      | `Finished of build_result
    ];
    key : B.Key.t;
    build_number : int64;           (* Increments on rebuild *)
    switch : Current.Switch.t;      (* Turning this off aborts the build. *)
    mutable auto_cancelled : bool;  (* Don't record the result (because the build was auto-cancelled). *)
    mutable ref_count : int;        (* The number of watchers waiting for the result (for auto-cancel). *)
  }

  let builds : build Builds.t ref = ref Builds.empty

  let is_running build =
    match build.job with
    | `Active _ -> true
    | `Finished _ -> false

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
  let do_build ~step ~job ctx build =
    let ready = !Job.timestamp () |> Unix.gmtime in
    Job.log job "Starting build for %a" B.pp build.key;
    let level = B.level ctx build.key in
    confirm ~job (Current.Step.confirmed level step, level) >>= fun () ->
    Lwt.catch
      (fun () -> B.build ~switch:build.switch ctx job build.key)
      (fun ex -> Lwt.return @@ Error (`Msg (Printexc.to_string ex)))
    >|= fun value ->
    let start_time =
      match Lwt.state (Job.start_time job) with
      | Lwt.Return time -> Some (Unix.gmtime time)
      | Lwt.Sleep when Stdlib.Result.is_ok value ->
        Log.warn (fun f -> f "Build %a succeeded, but never called set_running!" B.pp build.key);
        Fmt.failwith "Job.set_running not called!"
      | _ -> None
    in
    let end_time = !Job.timestamp () in
    let job_id = Job.id job in
    if build.auto_cancelled then (
      force_invalidate (B.Key.digest build.key);
      (* If anyone started wanting this build again after it got cancelled,
         then they will now recalculate, discover the build doesn't exist, and
         trigger a new one. *)
      { value = Error (`Msg "Auto-cancelled"); end_time; job_id }
    ) else (
      let record result =
        Db.Build.record
          ~builder:B.id
          ~build:build.build_number
          ~key:(B.Key.digest build.key)
          ~job:(Job.id job)
          ~ready ~running:start_time ~finished:(Unix.gmtime end_time)
          result;
        end_time
      in
      if Current.Switch.is_on build.switch then (
        match value with
        | Ok v ->
          Job.log job "Success";
          { value; end_time = record @@ Ok (B.Value.marshal v); job_id }
        | Error (`Msg m) as e ->
          Job.log job "Failed: %s" m;
          { value; end_time = record e; job_id }
      ) else (
        let value = Error (`Msg "Cancelled") in
        { value; end_time = record value; job_id }
      )
    )

  (* Create a new in-memory build object. Try to initialise it with the value from the disk-cache.
     If there isn't one, start a new build in a background thread. *)
  let get_build ~step ctx key =
    let key_digest = B.Key.digest key in
    let previous = Db.Build.lookup ~builder:B.id key_digest in
    match previous with
    | Some { Db.Build.rebuild = false; build = build_number; value; finished = end_time; job_id } ->
      Log.info (fun f -> f "@[<hov2>Loaded cached result for@ %a@]" B.pp key);
      let value = Stdlib.Result.map B.Value.unmarshal value in
      let switch = Current.Switch.create_off @@ Error (`Msg "Loaded from cache") in
      let job = `Finished { job_id; value; end_time } in
      { job; switch; build_number; key; ref_count = 0; auto_cancelled = false }
    | _ ->
      let finished, set_finished = Lwt.wait () in
      let build_number =
        match previous with
        | None -> 0L
        | Some x -> Int64.succ x.Db.Build.build       (* A rebuild was requested *)
      in
      let switch = Current.Switch.create ~label:(Fmt.strf "Build %a" B.pp key) () in
      let job = Job.create ~switch ~label:B.id () in
      let build = { job = `Active (job, finished); switch; build_number; key; ref_count = 0; auto_cancelled = false } in
      let finish build_result =
        build.job <- `Finished build_result;
        Lwt.wakeup set_finished build_result;
        Lwt.return_unit
      in
      Lwt.async (fun () ->
          Lwt.try_bind
            (fun () ->
               Lwt.finalize
                 (fun () -> do_build ~step ~job ctx build)
                 (fun () -> Current.Switch.turn_off build.switch @@ Ok ())
            )
            (fun result -> finish result)
            (fun ex ->
               finish {
                 job_id = Job.id job;
                 end_time = !Job.timestamp ();
                 value = Error (`Msg (Printexc.to_string ex))
               }
            )
        );
      build

  (* The build must be currently running when calling this. *)
  let input_running ~job ~build_result build =
    build.ref_count <- build.ref_count + 1;
    let cancel ~msg () =
      Lwt.async (fun () -> Current.Switch.turn_off build.switch @@ Error (`Msg msg))
    in
    let actions =
      object
        method pp f = B.pp f build.key
        method cancel = Some (cancel ~msg:"Cancelled by user")
        method rebuild = None
        method release =
          build.ref_count <- build.ref_count - 1;
          if build.ref_count = 0 && B.auto_cancel && is_running build then (
            build.auto_cancelled <- true;
            cancel ~msg:"Auto-cancelling job because it is no longer needed" ()
          )
      end
    in
    let start_time = Job.start_time job in
    let state, changed =
      match Lwt.state start_time with
      | Lwt.Sleep ->
        let changed = Lwt.choose [Lwt.map ignore build_result; Lwt.map ignore start_time] in
        `Ready, changed
      | Lwt.Return _ ->
        let changed = Lwt.choose [Lwt.map ignore build_result] in
        `Running, changed
      | Lwt.Fail ex -> raise ex
    in
    Error (`Active state), Current.Input.metadata ~job_id:(Current.Job.id job) ~changed actions

  (* The build must be finished when calling this. *)
  let input_finished ~build_result ~schedule build =
    let { job_id; value; end_time } = build_result in
    let rebuild () =
      invalidate build.key;
      Lwt_condition.broadcast rebuild_cond ()
    in
    let rebuild_requested = Lwt_condition.wait rebuild_cond in
    match schedule.Schedule.valid_for with
    | None ->
      (value :> B.Value.t Current_term.Output.t),
      Current.Input.metadata ~job_id ~changed:rebuild_requested @@
      object
        method pp f = B.pp f build.key
        method cancel = None
        method rebuild = Some rebuild
        method release = ()
      end
    | Some duration ->
      let remaining_time = Duration.to_f duration +. end_time -. !Job.timestamp () in
      let changed =
        if remaining_time <= 0.0 then (
          Log.info (fun f -> f "Build result for %a has expired" B.pp build.key);
          invalidate build.key;
          Lwt.return_unit       (* Trigger an immediate recalculation *)
        ) else (
          !Job.sleep remaining_time
        )
      in
      let changed = Lwt.choose [changed; rebuild_requested] in
      (value :> B.Value.t Current_term.Output.t),
      Current.Input.metadata ~job_id ~changed @@
      object
        method pp f =
          if remaining_time <= 0.0 then
            Fmt.pf f "%a (expired)" B.pp build.key
          else
            Fmt.pf f "%a will be invalid after %a"
              B.pp build.key
              pp_duration_rough (Duration.of_f remaining_time)

        method cancel = None
        method rebuild = Some rebuild
        method release = ()
      end

  let input build ~schedule =
    match build.job with
    | `Active (job, build_result) -> input_running ~job ~build_result build
    | `Finished build_result -> input_finished ~build_result ~schedule build

  let get ?(schedule=Schedule.default) ctx key =
    Current.Input.of_fn @@ fun step ->
    let key_digest = B.Key.digest key in
    let b =
      match Builds.find_opt key_digest !builds with
      | Some b -> b
      | None ->
        let b = get_build ~step ctx key in
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
    job : Job.t;
    finished : unit Lwt.t;
    mutable autocancelled : bool;   (* This op is expected to fail *)
  }

  type output = {
    key : Op.Key.t;
    mutable last_set : Current.Step.id;   (* Last evaluation step setting this output. *)
    mutable job_id : Current.job_id option; (* Current or last log *)
    mutable current : string option;      (* The current digest value, if known. *)
    mutable desired : Value.t;            (* The value we want. *)
    mutable ctx : Op.t;                   (* The context for [desired]. *)
    mutable op : [
      | `Active of op                     (* The currently-running operation. *)
      | `Error of [`Msg of string]        (* Why [desired] isn't possible. *)
      | `Finished of Op.Outcome.t         (* Note: if current <> desired then rebuild. *)
      | `Retry                            (* Need to try again. *)
    ];
  }

  let pp_op f (k, v) = Op.pp f (k, Value.value v)
  let pp_desired f output = pp_op f (output.key, output.desired)

  let outputs : output Outputs.t ref = ref Outputs.empty

  let pp_output f output =
    match output.op with
    | `Error (`Msg msg) ->
      Fmt.pf f "%a: %s" pp_desired output msg
    | `Finished _ ->
      Fmt.pf f "%a (completed)" pp_desired output
    | `Retry ->
      Fmt.pf f "%a (retry scheduled)" pp_desired output
    | `Active op ->
      if Value.equal op.value output.desired then
        Fmt.pf f "%a (in-progress)" pp_op (output.key, op.value)
      else
        Fmt.pf f "%a (stale), then %a"
          pp_op (output.key, op.value)
          pp_desired output

  (* Caller needs to notify about the change, if needed. *)
  let invalidate output =
    match output.op with
    | `Finished _ | `Retry ->
      output.current <- None;
      output.op <- `Retry;
      Db.Publish.invalidate ~op:Op.id (Op.Key.digest output.key)
    | _ -> assert false

  (* If output isn't in (or moving to) the desired state, start a thread to do that,
     unless we already tried that and failed. *)
  let rec maybe_start ~step output =
    match output.op with
    | `Error _ -> () (* Wait for error to be cleared. *)
    | `Active _ when not Op.auto_cancel ->
      (* Already publishing something and we don't auto-cancel.
         When the stale push completes, we'll get called again. *)
      ()
    | `Active op when Value.equal output.desired op.value ->
      (* We're already working to set the desired value. Just keep going. *)
      ()
    | `Active op ->
      assert Op.auto_cancel;
      Log.info (fun f -> f "Auto-cancelling %a" pp_op (output.key, op.value));
      op.autocancelled <- true;
      (* Cancel existing job. When that finishes, we'll get called again. *)
      Lwt.async (fun () ->
          Current.Switch.turn_off op.switch @@
          Error (`Msg "Auto-cancelling job because it is no longer needed")
        );
    | `Retry ->
        invalidate output;
        publish ~step output
    | `Finished _ ->
      match output.current with
      | Some current when current = Value.digest output.desired -> () (* Already the desired value. *)
      | _ ->
        (* Either we don't know the current state, or we know we want something different.
           We're not already running, and we haven't already failed. Time to publish! *)
        (* Once we start publishing, we don't know the state: *)
        invalidate output;
        publish ~step output
  and publish ~step output =
    let finished, set_finished = Lwt.wait () in
    let ctx = output.ctx in
    let switch = Current.Switch.create ~label:Op.id () in
    let job = Job.create ~switch ~label:Op.id () in
    let op = { value = output.desired; switch; job; finished; autocancelled = false } in
    output.op <- `Active op;
    Lwt.async
      (fun () ->
         Lwt.finalize
           (fun () ->
              let pp_op f = pp_op f (output.key, op.value) in
              output.job_id <- Some (Job.id job);
              Job.log job "Publish: %t" pp_op;
              Lwt.catch
                (fun () ->
                   let level = Op.level ctx output.key (Value.value op.value) in
                   confirm ~job (Current.Step.confirmed level step, level) >>= fun () ->
                   Op.publish ~switch ctx job output.key (Value.value op.value) >|= fun r ->
                   if Stdlib.Result.is_ok r && Lwt.state (Job.start_time job) = Lwt.Sleep then
                     Fmt.failwith "Job.set_running not called!";
                   r
                )
                (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))
              >|= function
              | Ok outcome ->
                Job.log job "Publish : succeeded";
                output.current <- Some (Value.digest op.value);
                output.op <- `Finished outcome;
                let job_id = Job.id job in
                Db.Publish.record ~op:Op.id ~job_id
                  ~key:(Op.Key.digest output.key)
                  ~value:(Value.digest op.value)
                  (Op.Outcome.marshal outcome);
                (* While we were pushing, we might have decided we wanted something else.
                   If so, start pushing that now. *)
                maybe_start ~step output;
                Lwt.wakeup set_finished ()
              | Error (`Msg m as e) ->
                if op.autocancelled then
                  Job.log job "Auto-cancel complete (%s)" m
                else
                  Job.log job "Publish failed: %s" m;
                let retry =
                  (* If it failed because we cancelled it, don't count that as an error. *)
                  op.autocancelled ||
                  (* If it failed but we have a new value to set, ignore the stale error. *)
                  not (Value.equal op.value output.desired)
                in
                output.op <- if retry then `Retry else `Error e;
                maybe_start ~step output;
                Lwt.wakeup set_finished ()
           )
           (fun () ->
              Current.Switch.turn_off switch @@ Ok ()
           )
      )

  (* Create a new in-memory [op], initialising it from the database. *)
  let get_op ~step_id ctx key desired =
    let current, job_id, op =
      match Db.Publish.lookup ~op:Op.id (Op.Key.digest key) with
      | Some { Db.Publish.value; job_id; outcome } ->
        Some value, Some job_id, `Finished (Op.Outcome.unmarshal outcome)
      | None -> None, None, `Retry
    in
    { key; current; desired; ctx; op; job_id; last_set = step_id }

  let set ctx key value =
    Current.Input.of_fn @@ fun step ->
    Log.debug (fun f -> f "set: %a" Op.pp (key, value));
    let key_digest = Op.Key.digest key in
    let value = Value.v value in
    let step_id = Current.Step.id step in
    let o =
      (* Ensure the [op] exists and has [op.desired = value]: *)
      match Outputs.find_opt key_digest !outputs with
      | Some o ->
        if o.last_set = step_id then
          Fmt.failwith "Error: output %a set to different values in the same step!" pp_op (key, value);
        o.last_set <- step_id;
        o.ctx <- ctx;
        if not (Value.equal value o.desired) then (
          o.desired <- value;
          match o.op with
          | `Error _ -> o.op <- `Retry   (* Clear error when the desired value changes. *)
          | `Active _ | `Finished _ | `Retry -> ()
        );
        o
      | None ->
        let o = get_op ~step_id ctx key value in
        outputs := Outputs.add key_digest o !outputs;
        o
    in
    maybe_start ~step o;
    let resolved x =
      match o.job_id with
      | None -> assert false
      | Some job_id ->
        let rebuild () =
          match o.op with
          | `Finished _ | `Error _ | `Retry ->
            o.op <- `Retry;
            invalidate o;
            Lwt_condition.broadcast rebuild_cond ()
          | `Active _ ->
            Log.info (fun f -> f "Rebuild(%a): already rebuilding" pp_op (key, value));
            ()
        in
        let changed = Lwt_condition.wait rebuild_cond in
        x, Current.Input.metadata ~job_id ~changed @@
        object
          method pp f = pp_output f o
          method cancel = None
          method rebuild = Some rebuild
          method release = ()
        end
    in
    match o.op with
    | `Finished x -> resolved (Ok x)
    | `Error e -> resolved (Error e :> Op.Outcome.t Current_term.Output.t)
    | `Retry -> resolved (Error (`Msg "(retry)")) (* (probably can't happen) *)
    | `Active op ->
      let a, changed =
        let started = Job.start_time op.job in
        if Lwt.state started = Lwt.Sleep then `Ready, Lwt.choose [Lwt.map ignore started; op.finished]
        else `Running, op.finished in
      Error (`Active a), Current.Input.metadata ?job_id:o.job_id ~changed @@
      object
        method pp f = pp_output f o
        method cancel = None
        method rebuild = None
        method release = ()
      end

  let reset () =
    outputs := Outputs.empty;
    Db.Publish.drop_all Op.id
end

module S = S

module Db = Db
