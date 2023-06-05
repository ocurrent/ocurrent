type 'a or_error = ('a, [`Msg of string]) result

module Config = Config

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "core"

  let evaluation_time_seconds =
    let help = "Total time spent evaluating" in
    Summary.v ~help ~namespace ~subsystem "evaluation_time_seconds"

  let pipeline_stage_total =
    let help = "Number of pipeline stages by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "pipeline_stage_total"
end

type job_id = string

class type actions = object
  method pp : Format.formatter -> unit
  method rebuild : (unit -> job_id) option
end

module Metadata = struct
  type t = {
    job_id : job_id option;
    update : Current_term.Output.active option;
  }
end

include Current_term.Make(Metadata)

module Primitive = struct
  type 'a t = 'a primitive

  let const x = Current_incr.const (Ok x, None)

  let map_result fn t =
    Current_incr.of_cc begin
      Current_incr.read t @@ fun (x, md) ->
      let y = try fn x with ex -> Error (`Msg (Printexc.to_string ex)) in
      Current_incr.write (y, md)
    end
end

type 'a term = 'a t

module Engine = struct
  (* Active jobs are ones which are referenced by the active pipeline.
     These are the only ones which can have actions attached.
     There are a list of actions objects for each job because the same job may
     appear multiple times in a pipeline. We only use the head object at any
     one time, but we need to cope with that disappearing. *)
  let active_jobs : actions list Job.Map.t ref = ref Job.Map.empty

  module Step = struct
    type t = < >
    let create () = object end
    let equal = (=)
    let current_step = ref (create ())
    let now () = !current_step
    let advance () =
      current_step := create ()
  end

  type results = {
    value : unit Current_term.Output.t;
    jobs : actions Job.Map.t;
  }

  type t = {
    thread : 'a. 'a Eio.Promise.or_exn;
    last_result : results ref;
    pipeline : unit term Lazy.t;
    config : Config.t;
  }

  (* Functions to call at end-of-propagate.
     When an incremental calculation that performed some side-effect (e.g.
     incrementing a ref-counter) needs to be re-done, we add the compensating
     operation here. We run them only once the propagation is complete so that
     if the replacement computation also increments the ref-counter then we
     won't destroy the job just to recreate it immediately. *)
  let release_queue = Queue.create ()

  let rec flush_release_queue () =
    match Queue.take_opt release_queue with
    | None -> ()
    | Some fn ->
      fn ();
      flush_release_queue ()

  let propagate = Eio.Condition.create ()

  let update () =
    Eio.Condition.broadcast propagate

  let booting = {
    value = Error (`Active `Running);
    jobs = Job.Map.empty;
  }

  let default_trace ~next:_ _ = ()

  let pipeline t = Lazy.force t.pipeline

  let create ?(config=Config.default) ?(trace=default_trace) ~sw f =
    let last_result = ref booting in
    let rec aux outcome =
      let next = Eio.Fiber.fork_promise ~sw (fun () -> Eio.Condition.await_no_mutex propagate) in
      let t0 = Unix.gettimeofday () in
      Current_incr.propagate ();
      let t1 = Unix.gettimeofday () in
      Prometheus.Summary.observe Metrics.evaluation_time_seconds (t1 -. t0);
      (* Release all the old resources, now we've had a chance to create any replacements. *)
      flush_release_queue ();
      let r = Current_incr.observe outcome in
      if not (Current_term.Output.equal Unit.equal r !last_result.value) then
        Log.info (fun f -> f "Result: %a" Current_term.(Output.pp Fmt.(any "()")) r);
      last_result := {
        value = r;
        jobs = Job.Map.map List.hd !active_jobs;
      };
      trace ~next !last_result;
      Log.debug (fun f -> f "Waiting for an external event...");
      Eio.Promise.await_exn next;
      Eio.Fiber.yield ();
      Step.advance ();
      aux outcome
    in
    let pipeline = lazy (f ()) in
    let thread =
      Eio.Fiber.fork_promise ~sw (fun () ->
      (* The pause lets us start the web-server before the first evaluation,
         and also frees us from handling an initial exception specially. *)
      Eio.Fiber.yield ();
      if Current_incr.observe Config.now <> None then
        failwith "Engine is already running (Config.now already set)!";
      Current_incr.change Config.active_config (Some config);
      Fun.protect
        (fun () ->
          try aux (Executor.run (Lazy.force pipeline))
          with ex -> (
            if ex = Exit then (
              (* Clean up, for unit-tests *)
              Current_incr.propagate ();
              flush_release_queue ();
            );
            raise ex
          )
        )
        ~finally:(fun () -> Current_incr.change Config.active_config None)
      )
    in
    { thread; last_result; config; pipeline }

  let on_disable fn =
    Current_incr.on_release @@ fun () ->
    Queue.add fn release_queue

  let state t = !(t.last_result)

  let jobs s = s.jobs

  let config t = t.config

  let thread t = t.thread

  let update_metrics _t =
    (*  { Current_term.S.ok; ready; running; failed; blocked } = Analysis.stats (pipeline t) in *)
    let { Current_term.S.ok; waiting_for_confirmation; ready; running; failed; blocked } = Analysis.quick_stat () in
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ok") (float_of_int ok);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "waiting_for_confirmation") (float_of_int waiting_for_confirmation);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ready") (float_of_int ready);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "running") (float_of_int running);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "failed") (float_of_int failed);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "blocked") (float_of_int blocked)
end

module Var (T : Current_term.S.T) = struct
  type t = {
    current : T.t Current_term.Output.t Current_incr.var;
    name : string;
  }

  let create ~name current =
    let current = Current_incr.var current in
    { current; name }

  let get t =
    let open Syntax in
    component "%s" t.name |>
    let> () = return () in
    Current_incr.of_cc begin
      Current_incr.read (Current_incr.of_var t.current) @@ fun v ->
      Current_incr.write (v, None)
    end

  let set t v =
    Current_incr.change t.current v;
    Engine.update ()

  let update t f =
    Current_incr.change t.current (f (Current_incr.observe (Current_incr.of_var t.current)));
    Engine.update ()
end

module Monitor = struct
  type 'a t = {
    sw : Eio.Switch.t;
    read : unit -> 'a or_error Eio.Promise.t;
    watch : (unit -> unit) -> (unit -> unit);
    pp : Format.formatter -> unit;
    value : 'a Current_term.Output.t Current_incr.var;
    reading : bool Current_incr.var;      (* Is a read operation in progress? *)
    mutable ref_count : int;              (* Number of terms using this monitor *)
    mutable need_refresh : bool;          (* Update detected after current read started *)
    mutable active : bool;                (* Monitor thread is running *)
    cond : Eio.Condition.t;          (* Maybe time to leave the "wait" state *)
  }

  let catch t (fn : unit -> 'a or_error Eio.Promise.t) : 'a or_error =
    try ((Eio.Promise.await @@ fn ()) :> 'a or_error) with ex ->
      Log.warn (fun f -> f "Uncaught exception in monitor %t: %a" t.pp Fmt.exn ex);
      Error (`Msg (Printexc.to_string ex))

  let refresh t () =
    t.need_refresh <- true;
    Eio.Condition.broadcast t.cond

  let rec enable t =
    let unwatch = t.watch (refresh t) in
    if t.ref_count = 0 then disable ~unwatch t
    else get_value t ~unwatch
  and disable ~unwatch t =
    unwatch ();
    if t.ref_count > 0 then enable t
    else (
      assert t.active;
      t.active <- false;
      (* Clear the saved value, so that if we get activated again then we don't
         start by serving up the previous value, which could be quite stale by then. *)
      Current_incr.change t.value @@ Error (`Active `Running);
      `Finished
    )
  and get_value ~unwatch t =
    t.need_refresh <- false;
    Current_incr.change t.reading true;
    Engine.update ();
    let v = catch t t.read in
    Current_incr.change t.reading false;
    Current_incr.change t.value @@ (v :> _ Current_term.Output.t);
    Engine.update ();
    wait ~unwatch t
  and wait ~unwatch t =
    if t.ref_count = 0 then disable ~unwatch t
    else if t.need_refresh then get_value ~unwatch t
    else begin
      Eio.Condition.await_no_mutex t.cond;
      wait ~unwatch t
    end

  let get t =
    Current_incr.of_cc begin
      t.ref_count <- t.ref_count + 1;
      Engine.on_disable (fun () ->
          assert (t.ref_count > 0);
          t.ref_count <- t.ref_count - 1;
          if t.ref_count = 0 then Eio.Condition.broadcast t.cond
        );
      if not t.active then (
        t.active <- true;
        Eio.Fiber.fork ~sw:t.sw (fun () ->
            (* [pause] to ensure we're outside of any existing propagate here. *)
            Eio.Fiber.yield ();
            enable t |> fun `Finished -> ()
          )
      );  (* (else the previous thread will check [ref_count] before exiting) *)
      Current_incr.read (Current_incr.of_var t.value) @@ fun value ->
      Current_incr.read (Current_incr.of_var t.reading) @@ fun reading ->
      let update = if reading then Some `Running else None in
      let metadata = { Metadata.job_id = None; update } in
      Current_incr.write (value, Some metadata)
    end

  let create ~sw ~read ~watch ~pp =
    let cond = Eio.Condition.create () in
    {
      sw;
      ref_count = 0;
      active = false;
      need_refresh = true;
      cond;
      reading = Current_incr.var false;
      value = Current_incr.var (Error (`Active `Running));
      read; watch; pp
    }
end

module Level = Level

module String = struct
  type t = string
  let digest t = t
  let pp = Fmt.string
  let marshal t = t
  let unmarshal t = t
  let equal = String.equal
end

module Unit = struct
  type t = unit

  let pp f () = Fmt.string f "()"
  let compare () () = 0
  let digest () = ""
  let equal () () = true
  let marshal () = "()"
  let unmarshal = function
    | "()" -> ()
    | x -> Fmt.failwith "Unit.unmarshal(%S)" x
end

let state_dir = Disk_store.state_dir

module Db = Db
module Process = Process
module Pool = Pool
module Log_matcher = Log_matcher

module Job = struct
  include Job

  let register_actions job_id actions =
    let add = function
      | None -> Some [actions]
      | Some xs -> Some (actions :: xs)
    in
    let remove xs =
      let rec aux = function
        | [] -> assert false
        | x :: xs when x == actions -> xs
        | x :: xs -> x :: aux xs
      in
      match aux (Option.get xs) with
      | [] -> None
      | xs -> Some xs
    in
    Engine.active_jobs := Job.Map.update job_id add !Engine.active_jobs;
    Engine.on_disable @@ fun () ->
    Engine.active_jobs := Job.Map.update job_id remove !Engine.active_jobs
end
