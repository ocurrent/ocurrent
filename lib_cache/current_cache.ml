open Lwt.Infix

module Job = Current.Job

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "cache"

  let memory_cache_items =
    let help = "Number of results cached in RAM" in
    Gauge.v_label ~label_name:"id" ~help ~namespace ~subsystem "memory_cache_items"

  let evaluations_total =
    let help = "Number of evaluations performed" in
    Counter.v ~help ~namespace ~subsystem "evaluations_total"
end

(* For each live job, the (op, key) of the database entry it will create.
   This is used to show build history for live jobs, which aren't yet in the database. *)
let key_of_job_id = Hashtbl.create 10
let job_id_of_key = Hashtbl.create 10

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

  type op = {
    value : Value.t;                (* The value currently being set. *)
    job : Job.t;
    mutable autocancelled : bool;   (* This op is expected to fail *)
  }

  type output = {
    key : Op.Key.t;
    mutable build_number : int64;         (* Number of recorded (incl failed) builds with this key. *)
    mutable ref_count : int;              (* The number of watchers waiting for the result (for auto-cancel). *)
    mutable last_set : Current.Engine.Step.t; (* Last evaluation step setting this output. *)
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
    mutable mtime : float;                (* Time last operation completed (if finished or error). *)
    mutable expires : (float * unit Lwt.t) option;     (* Time and sleeping thread (for cancellation). *)
    notify : unit Current_incr.var;       (* Async thread sets this to update results. *)
  }

  (* State model:

     The first time a key is used, a new output is created in the Retry state.
     Whenever an output is wanted and we are in Retry, we transition to Active
     and start the Lwt builder process. The only way to leave Active is by the
     Lwt process finishing.

     While Active, we may flag that the value we are setting is out-of-date,
     that the user wants to cancel, or that we should cancel because the build
     is no longer needed. But we still wait for the process to finish,
     possibly encouraging it using [Job.cancel].

     When an Active job finishes:

     - If it was auto-cancelled then we discard the result and return to Retry.
     - Otherwise we store the result on disk.
     - If we need to rebuild (because the user asked for another value during the build)
       then we return to Retry.
     - Otherwise, we move to Finished or Error, depending on whether the operation succeeded.

     In Finished or Error, the user can trigger a rebuild, moving us back to Retry.
     Also, if the user sets a different value then we move to Retry.

     The user can only ask to cancel while we are Active. They can only ask to Rebuild
     when Finished or Error.
   *)

  let pp_op f (k, v) = Op.pp f (k, Value.value v)
  let pp_desired f output = pp_op f (output.key, output.desired)

  (* The in-memory cache of outputs. *)
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
  let invalidate_output output =
    match output.op with
    | `Finished _ | `Retry ->
      output.current <- None;
      output.op <- `Retry;
      Db.invalidate ~op:Op.id (Op.Key.digest output.key)
    | _ -> assert false

  (* Caller needs to notify about the change, if needed. *)
  let invalidate key =
    let key = Op.Key.digest key in
    match Outputs.find_opt key !outputs with
    | Some o -> invalidate_output o
    | None -> Db.invalidate ~op:Op.id key

  let notify t =
    Current_incr.change t.notify () ~eq:(fun _ _ -> false);
    Current.Engine.update ()

  (* If output isn't in (or moving to) the desired state, start a thread to do that,
     unless we already tried that and failed. Only call this if the output is currently
     wanted. If called from an async thread, you must call [notify] afterwards too. *)
  let rec maybe_start ~config output =
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
      Job.cancel op.job "Auto-cancelling job because it is no longer needed"
    | `Retry ->
        invalidate_output output;
        publish ~config output
    | `Finished _ ->
      match output.current with
      | Some current when current = Value.digest output.desired -> () (* Already the desired value. *)
      | _ ->
        (* Either we don't know the current state, or we know we want something different.
           We're not already running, and we haven't already failed. Time to publish! *)
        (* Once we start publishing, we don't know the state: *)
        invalidate_output output;
        publish ~config output
  and maybe_restart ~config output =
    maybe_start ~config output;
    notify output
  and publish ~config output =
    let ctx = output.ctx in
    let switch = Current.Switch.create ~label:Op.id () in
    let job = Job.create ~switch ~label:Op.id ~config () in
    let job_id = Job.id job in
    output.job_id <- Some job_id;
    let op = { value = output.desired; job; autocancelled = false } in
    let ready = !Job.timestamp () |> Unix.gmtime in
    output.op <- `Active op;
    let pp_op f = pp_op f (output.key, op.value) in
    Job.log job "New job: %t" pp_op;
    Lwt.async
      (fun () ->
         Job.start_time job >>= fun _ ->
         Lwt.pause () >|= fun () ->        (* Ensure we're outside any propagate *)
         notify output
      );
    let key_digest = Op.Key.digest output.key in
    Hashtbl.add key_of_job_id job_id (Op.id, key_digest);
    Hashtbl.add job_id_of_key (Op.id, key_digest) job_id;
    Lwt.async
      (fun () ->
         Lwt.finalize
           (fun () ->
              Lwt.catch
                (fun () -> Op.publish ctx job output.key (Value.value op.value))
                (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))
              >>= fun outcome ->
              Lwt.pause () >|= fun () ->        (* Ensure we're outside any propagate *)
              let end_time = Unix.gmtime @@ !Job.timestamp () in
              if op.autocancelled then (
                output.op <- `Retry;
                invalidate_output output
              ) else (
                (* Record the result *)
                let running =
                  match Lwt.state (Job.start_time job) with
                  | Lwt.Return x -> Some (Unix.gmtime x)
                  | Lwt.Sleep when Stdlib.Result.is_ok outcome -> Fmt.failwith "Job.start not called!";
                  | _ -> None
                in
                let outcome =
                  match Current.Job.cancelled_state op.job, outcome with
                  | Error _cancelled, _ -> Error (`Msg "Cancelled")
                  | Ok (), Ok _ -> Job.log job "Job succeeded"; outcome
                  | Ok (), Error (`Msg m) ->
                    Job.log job "Job failed: %s" m;
                    match Current.Log_matcher.analyse_job job with
                    | None -> outcome
                    | Some e -> Error (`Msg e)
                in
                output.mtime <- !Job.timestamp ();
                begin match outcome with
                  | Ok outcome ->
                    output.current <- Some (Value.digest op.value);
                    output.op <- `Finished outcome;
                  | Error e ->
                    (* If it failed but we have a new value to set, ignore the stale error. *)
                    let retry = not (Value.equal op.value output.desired) in
                    output.op <- if retry then `Retry else `Error e
                end;
                let job_id = Job.id job in
                let outcome = Stdlib.Result.map Op.Outcome.marshal outcome in
                Db.record ~op:Op.id ~job_id
                  ~key:key_digest
                  ~value:(Value.digest op.value)
                  ~ready ~running ~finished:end_time
                  ~build:output.build_number
                  outcome;
                output.build_number <- Int64.succ output.build_number;
              )
           )
           (fun () ->
              Hashtbl.remove key_of_job_id (Job.id job);
              Hashtbl.remove job_id_of_key (Op.id, key_digest);
              Current.Switch.turn_off switch >|= fun () ->
              (* While we were working, we might have decided we wanted something else.
                 If so, start that now. *)
              if output.ref_count > 0 then maybe_restart ~config output
           )
      )

  let limit_expires t time =
    let set () =
      let remaining_time = time -. !Job.timestamp () in
      let thread =
        Lwt.catch
          (fun () ->
             !Job.sleep remaining_time >>= fun () ->
             Lwt.pause () >|= fun () ->        (* Ensure we're outside any propagate *)
             invalidate t.key;
             let config = Option.get (Current_incr.observe Current.Config.now) in
             maybe_restart ~config t
          )
          (function
            | Lwt.Canceled ->
              Lwt.return_unit
            | ex ->
              Log.err (fun f -> f "Expiry thread failed: %a" Fmt.exn ex);
              Lwt.return_unit
          )
      in
      t.expires <- Some (time, thread)
    in
    match t.expires with
    | Some (prev, _) when prev <= time -> ()            (* Already expiring by [time] *)
    | Some (_, thread) -> Lwt.cancel thread; set ()
    | None -> set ()

  let cancel_expires t =
    match t.expires with
    | Some (_, thread) -> Lwt.cancel thread; t.expires <- None
    | None -> ()

  (* Create a new in-memory output, initialising it from the database. *)
  let get_output ~step_id ctx key desired =
    let current, job_id, op, mtime, build_number =
      match Db.lookup ~op:Op.id (Op.Key.digest key) with
      | Some { Db.value; job_id; outcome; finished; build; rebuild; _ } ->
        let op = match outcome with
          | _ when rebuild -> `Retry
          | Error e -> `Error e
          | Ok outcome ->
            try `Finished (Op.Outcome.unmarshal outcome)
            with ex ->
              Log.warn (fun f -> f "Failed to restore %S cached outcome: %a (will rebuild)" Op.id Fmt.exn ex);
              `Retry
        in
        Some value, Some job_id, op, finished, Int64.succ build
      | None -> None, None, `Retry, Unix.gettimeofday (), 0L
    in
    let notify = Current_incr.var () in
    { key; current; desired; ctx; op; job_id; last_set = step_id;
      ref_count = 0; mtime; build_number; notify; expires = None }

  (* Register the actions for a resolved (non-active) output.
     Report it as changed when a rebuild is requested (manually or via the schedule). *)
  let register_resolved o ~schedule ~value ~config =
    let key = o.key in
    match o.job_id with
    | None -> assert false
    | Some job_id ->
      let rebuild () =
        match o.op with
        | `Finished _ | `Error _ | `Retry ->
          o.op <- `Retry;
          maybe_restart ~config o;
          Option.get o.job_id
        | `Active _ ->
          Log.info (fun f -> f "Rebuild(%a): already rebuilding" pp_op (key, value));
          Option.get o.job_id
      in
      match schedule.Schedule.valid_for with
      | None ->
        Current.Job.register_actions job_id @@ object
          method pp f = pp_output f o
          method rebuild = Some rebuild
        end
      | Some duration ->
        let expires = Duration.to_f duration +. o.mtime in
        let remaining_time = expires -. !Job.timestamp () in
        if remaining_time <= 0.0 then (
          Log.info (fun f -> f "Result for %a has expired" pp_op (key, value));
          invalidate key;
          notify o   (* Trigger an immediate recalculation *)
        ) else (
          limit_expires o expires
        );
        Current.Job.register_actions job_id @@
        object
          method pp f =
            if remaining_time <= 0.0 then
              Fmt.pf f "%a (expired)" pp_op (key, value)
            else
              Fmt.pf f "%a will be invalid after %a"
                pp_op (key, value)
                pp_duration_rough (Duration.of_f remaining_time)
          method rebuild = Some rebuild
        end

  let register_actions ~schedule ~value ~config o =
    match o.op with
    | `Finished _ | `Error _ | `Retry -> register_resolved ~schedule ~value ~config o
    | `Active _ ->
      cancel_expires o;
      o.ref_count <- o.ref_count + 1;
      Current.Engine.on_disable (fun () ->
          o.ref_count <- o.ref_count - 1;
          if o.ref_count = 0 && Op.auto_cancel then (
            match o.op with
            | `Active op ->
              op.autocancelled <- true;
              Job.cancel op.job "Auto-cancelling job because it is no longer needed"
            | _ -> ()
          )
        );
      Current.Job.register_actions (Option.get o.job_id) @@ object
        method pp f = pp_output f o
        method rebuild = None
      end

  let set ?(schedule=Schedule.default) ctx key value =
    Current_incr.of_cc begin
      Current_incr.read Current.Config.now @@ function
      | None -> Current_incr.write (Error (`Active `Ready), None)
      | Some config ->
        Log.debug (fun f -> f "set: %a" Op.pp (key, value));
        let key_digest = Op.Key.digest key in
        let value = Value.v value in
        let step_id = Current.Engine.Step.now () in
        (* Ensure the output exists and has [o.desired = value]: *)
        let o =
          match Outputs.find_opt key_digest !outputs with
          | Some o ->
            (* Output already exists in the memory cache. Update it if needed. *)
            let changed = not (Value.equal value o.desired) in
            if o.last_set = step_id then (
              if changed then
                Fmt.failwith "Error: output %a set to different values in the same step!" pp_op (key, value);
            ) else (
              o.last_set <- step_id;
              o.ctx <- ctx;
              if changed then (
                o.desired <- value;
                match o.op with
                | `Error _ -> o.op <- `Retry   (* Clear error when the desired value changes. *)
                | `Active _ | `Finished _ | `Retry -> ()
              );
            );
            o
          | None ->
            (* Not in memory cache. Restore from disk if available, or create a new output if not.
               Either way, [o.desired] is set to [value]. *)
            let o = get_output ~step_id ctx key value in
            outputs := Outputs.add key_digest o !outputs;
            Prometheus.Gauge.inc_one (Metrics.memory_cache_items Op.id);
            o
        in
        (* Ensure a build is in progress if we need one: *)
        maybe_start ~config o;
        (* Read from [o.notify] so that we re-evaluate the following when something changes. *)
        Current_incr.read (Current_incr.of_var o.notify) @@ fun () ->
        Prometheus.Counter.inc_one Metrics.evaluations_total;
        register_actions ~config ~schedule ~value o;
        (* Return the current state: *)
        let v =
          match o.op with
          | `Finished x -> Ok x
          | `Error e -> (Error e :> Op.Outcome.t Current_term.Output.t)
          | `Retry -> Error (`Active `Running)
          | `Active op ->
            let a =
              let started = Job.start_time op.job in
              if Lwt.state started = Lwt.Sleep then `Ready else `Running
            in
            Error (`Active a)
        in
        let metadata = { Current.Metadata.job_id = o.job_id; update = None } in
        Current_incr.write (v, Some metadata)
    end

  let reset () =
    outputs := Outputs.empty;
    Prometheus.Gauge.set (Metrics.memory_cache_items Op.id) 0.0;
    Db.drop_all Op.id
end

module Make(B : S.BUILDER) = struct
  module Adaptor = struct
    type t = B.t

    let id = B.id

    module Key = B.Key
    module Value = Current.Unit
    module Outcome = B.Value

    let publish op job key () =
      B.build op job key

    let pp f (key, ()) = B.pp f key

    let auto_cancel = B.auto_cancel
  end

  include Output(Adaptor)

  let get ?schedule ctx key =
    set ?schedule ctx key ()
end

module S = S

module Db = struct
  include Db

  let history ~limit ~job_id =
    let key =
      match Hashtbl.find_opt key_of_job_id job_id with
      | None -> Db.lookup_job_id job_id
      | Some _ as k -> k
    in
    match key with
    | None -> None, []
    | Some (op, key) ->
      let complete = Db.history ~limit ~op key in
      let active = Hashtbl.find_opt job_id_of_key (op, key) in
      active, complete
end
