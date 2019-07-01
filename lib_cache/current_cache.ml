open Lwt.Infix
open Current.Syntax

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

module Make(B : S.BUILDER with type job := Job.t) = struct
  module Builds = Map.Make(String)

  type build_result = B.Value.t Current.or_error * float  (* The final result and end time *)

  type build = {
    key : B.Key.t;
    switch : Lwt_switch.t;          (* Turning this off aborts the build. *)
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
  let do_build ~confirmed ctx build =
    let job = Job.create ~switch:build.switch ~id:B.id () in
    let log = Job.log_id job in
    let ready = !timestamp () |> Unix.gmtime in
    let running = ref None in
    let is_finished = ref false in
    Job.log job "Starting build for %a" B.pp build.key;
    Lwt_switch.add_hook (Some build.switch) (fun () ->
        if build.auto_cancelled then (
          Job.log job "Auto-cancelling job because it is no longer needed";
        ) else if not !is_finished then (
          Job.log job "Cancelling job"
        );
        Lwt.return_unit
      );
    confirm confirmed >>= fun () ->
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
          ~key:(B.Key.digest build.key)
          ~log
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
  let get_build ~confirmed ctx key =
    let key_digest = B.Key.digest key in
    let switch = Lwt_switch.create () in
    let finished, set_finished = Lwt.wait () in
    let build = { switch; key; finished; ref_count = 0; auto_cancelled = false } in
    match Db.Build.lookup ~builder:B.id key_digest with
    | Some entry ->
      Log.info (fun f -> f "Loaded cached result for %a" B.pp key);
      let v =
        match entry.Db.Build.value with
        | Ok v -> Ok (B.Value.unmarshal v)
        | Error _ as e -> e
      in
      Lwt.wakeup set_finished (v, entry.Db.Build.finished);
      build
    | None ->
      Lwt.async (fun () ->
          Lwt.try_bind
            (fun () ->
               Lwt.finalize
                 (fun () -> do_build ~confirmed ctx build)
                 (fun () -> Lwt_switch.turn_off build.switch)
            )
            (fun result -> Lwt.wakeup set_finished result; Lwt.return_unit)
            (fun ex -> Lwt.wakeup_exn set_finished ex; Lwt.return_unit)
        );
      build

  (* The build must be currently running when calling this. *)
  let input_running build =
    build.ref_count <- build.ref_count + 1;
    let cancel () = Lwt.async (fun () -> Lwt_switch.turn_off build.switch) in
    let watch =
      object
        method pp f = B.pp f build.key
        method changed = Lwt.map ignore build.finished
        method cancel = Some cancel
        method release =
          build.ref_count <- build.ref_count - 1;
          if build.ref_count = 0 && B.auto_cancel && is_running build then (
            build.auto_cancelled <- true;
            cancel ()
          )
      end
    in
    Error `Pending, [watch]

  (* The build must be finished when calling this. *)
  let input_finished ~schedule build (value, finished) =
    match schedule.Schedule.valid_for with
    | None -> (value :> B.Value.t Current_term.Output.t), []
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
      (value :> B.Value.t Current_term.Output.t), [watch]

  let input build ~schedule () =
    match Lwt.state build.finished with
    | Lwt.Sleep -> input_running build
    | Lwt.Return v -> input_finished build ~schedule v
    | Lwt.Fail ex -> raise ex

  let get ?(schedule=Schedule.default) ctx key =
    let level = B.level ctx key in
    let* confirmed = Current.confirmed level in
    Current.track @@
    let key_digest = B.Key.digest key in
    let b =
      match Builds.find_opt key_digest !builds with
      | Some b -> b
      | None ->
        let b = get_build ~confirmed:(confirmed, level) ctx key in
        builds := Builds.add key_digest b !builds;
        b
    in
    Current.Input.of_fn (input b ~schedule)

  let reset () =
    builds := Builds.empty;
    Db.Build.drop_all B.id
end

module Output(Op : S.PUBLISHER with type job := Job.t) = struct
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
    switch : Lwt_switch.t;          (* Turning this off aborts the operation. *)
    finished : unit Lwt.t;
    mutable autocancelled : bool;   (* This op is expected to fail *)
  }

  type output = {
    key : Op.Key.t;
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
  let rec maybe_start ~confirmed output =
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
      Lwt.async (fun () -> Lwt_switch.turn_off op.switch)
    | `Finished ->
      match output.current with
      | Some current when current = Value.digest output.desired -> () (* Already the desired value. *)
      | _ ->
        (* Either we don't know the current state, or we know we want something different.
           We're not already running, and we haven't already failed. Time to publish! *)
        (* Once we start publishing, we don't know the state: *)
        output.current <- None;
        Db.Publish.invalidate ~op:Op.id (Op.Key.digest output.key);
        output.op <- `Running (publish ~confirmed output)
  and publish ~confirmed output =
    let switch = Lwt_switch.create () in
    let finished, set_finished = Lwt.wait () in
    let ctx = output.ctx in
    let op = { value = output.desired; switch; finished; autocancelled = false } in
    Lwt.async
      (fun () ->
         let pp_op f = pp_op f (output.key, op.value) in
         let job = Job.create ~switch ~id:Op.id () in
         Job.log job "Publish: %t" pp_op;
         Lwt.catch
           (fun () ->
              confirm confirmed >>= fun () ->
              Op.publish ~switch ctx job output.key (Value.value op.value)
           )
           (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))
         >|= function
         | Ok () ->
           Job.log job "Publish(%t) : succeeded" pp_op;
           output.current <- Some (Value.digest op.value);
           output.op <- `Finished;
           (* While we were pushing, we might have decided we wanted something else.
              If so, start pushing that now. *)
           maybe_start ~confirmed output;
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
           maybe_start ~confirmed output;
           Lwt.wakeup set_finished ()
      );
    op

  (* Create a new in-memory [op], initialising it from the database. *)
  let get_op ctx key desired =
    let current =
      match Db.Publish.lookup ~op:Op.id (Op.Key.digest key) with
      | Some entry -> Some entry.Db.Publish.value
      | None -> None
    in
    { key; current; desired; ctx; op = `Finished }

  let set ctx key value =
    let level = Op.level ctx key in
    let* confirmed = Current.confirmed level in
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
    maybe_start ~confirmed:(confirmed, level) o;
    Current.track @@
    Current.Input.of_fn @@ fun () ->
    match o.op with
    | `Finished -> Ok (), []
    | `Error e -> (Error e :> unit Current_term.Output.t), []
    | `Running op ->
      let watch =
        object
          method pp f = pp_output f o
          method changed = op.finished
          method cancel = None
          method release = ()
        end
      in
      Error `Pending, [watch]

  let reset () =
    outputs := Outputs.empty;
    Db.Publish.drop_all Op.id
end

module Job = Job
module Process = Process
module S = S
