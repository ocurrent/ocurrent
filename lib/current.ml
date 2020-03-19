open Lwt.Infix

type 'a or_error = ('a, [`Msg of string]) result

module Config = Config

module Job_map = Job.Map

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
  method release : unit
end

type job_metadata = {
  job_id : job_id option;
  actions : actions;
}

let watches : actions list ref = ref []         (* Will call #release at end-of-step *)
let active_jobs : actions Job_map.t ref = ref Job_map.empty

module Input = struct
  type nonrec job_id = job_id

  type 'a t = unit -> 'a Current_term.Output.t * job_id option

  let register_actions ?job_id actions =
    watches := actions :: !watches;
    begin match job_id with
      | None -> ()
      | Some job_id -> active_jobs := Job_map.add job_id actions !active_jobs
    end

  let of_fn f step =
    try f step
    with ex ->
      Log.warn (fun f -> f "Uncaught exception from input: %a" Fmt.exn ex);
      Error (`Msg (Printexc.to_string ex)), None

  let const x =
    of_fn @@ fun () ->
    Ok x, None

  let get (t : 'a t) = t ()

  let map_result fn t step =
    let x, md = t step in
    let y = try fn x with ex -> Error (`Msg (Printexc.to_string ex)) in
    y, md
end

include Current_term.Make(Input)

type 'a term = 'a t

module Engine = struct
  module Step = struct
    type t = < >
    let create () = object end
    let equal = (=)
    let current_step = ref (create ())
    let now () = !current_step
    let advance () =
      current_step := create ()
  end

  type metadata = job_metadata

  type results = {
    value : unit Current_term.Output.t;
    analysis : Analysis.t;
    jobs : actions Job_map.t;
  }

  type t = {
    thread : 'a. 'a Lwt.t;
    last_result : results ref;
    config : Config.t;
  }

  let propagate = Lwt_condition.create ()

  let update () =
    Lwt_condition.broadcast propagate ()

  let booting = {
    value = Error (`Active `Running);
    analysis = Analysis.booting;
    jobs = Job_map.empty;
  }

  let default_trace ~next:_ r =
    Log.info (fun f -> f "Result: %a" Current_term.(Output.pp Fmt.(unit "()")) r.value);
    Lwt.return_unit

  let create ?(config=Config.default) ?(trace=default_trace) f =
    let last_result = ref booting in
    let rec aux old_watches =
      let next = Lwt_condition.wait propagate in
      Log.debug (fun f -> f "Evaluating...");
      let t0 = Unix.gettimeofday () in
      let r, an = Executor.run f in
      let t1 = Unix.gettimeofday () in
      Prometheus.Summary.observe Metrics.evaluation_time_seconds (t1 -. t0);
      let new_jobs = !active_jobs in
      active_jobs := Job_map.empty;
      let new_watches = !watches in
      watches := [];
      List.iter (fun w -> w#release) old_watches;
      last_result := {
        value = r;
        analysis = an;
        jobs = new_jobs;
      };
      trace ~next !last_result >>= fun () ->
      Log.debug (fun f -> f "Waiting for inputs to change...");
      next >>= fun () ->
      Lwt.pause () >>= fun () ->
      Step.advance ();
      aux new_watches
    in
    let thread =
      (* The pause lets us start the web-server before the first evaluation,
         and also frees us from handling an initial exception specially. *)
      Lwt.pause () >>= fun () ->
      if !Config.now <> None then failwith "Engine is already running (Config.now already set)!";
      Config.now := Some config;
      Lwt.finalize
        (fun () -> aux [])
        (fun () -> Config.now := None; Lwt.return_unit)
    in
    { thread; last_result; config }

  let state t = !(t.last_result)

  let jobs s = s.jobs

  let config t = t.config

  let thread t = t.thread

  let actions m = m.actions

  let job_id m = m.job_id

  let pp_metadata f m = m.actions#pp f

  let update_metrics results =
    let { Current_term.S.ok; ready; running; failed; blocked } = Analysis.stats results.analysis in
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ok") (float_of_int ok);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "ready") (float_of_int ready);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "running") (float_of_int running);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "failed") (float_of_int failed);
    Prometheus.Gauge.set (Metrics.pipeline_stage_total "blocked") (float_of_int blocked)
end

module Var (T : Current_term.S.T) = struct
  type t = {
    mutable current : T.t Current_term.Output.t;
    name : string;
  }

  let create ~name current =
    { current; name }

  let get t =
    let open Syntax in
    component "%s" t.name |>
    let> () = return () in
    Input.of_fn @@ fun _env ->
    t.current, None

  let set t v =
    t.current <- v;
    Engine.update ()

  let update t f =
    t.current <- f t.current;
    Engine.update ()
end

module Monitor = struct
  type 'a t = {
    read : unit -> 'a or_error Lwt.t;
    watch : (unit -> unit) -> (unit -> unit Lwt.t) Lwt.t;
    pp : Format.formatter -> unit;
    mutable value : 'a Current_term.Output.t;
    mutable ref_count : int;              (* Number of terms using this input *)
    mutable need_refresh : bool;          (* Update detected after current read started *)
    mutable active : bool;                (* Monitor thread is running *)
    cond : unit Lwt_condition.t;          (* Maybe time to leave the "wait" state *)
  }

  let refresh t () =
    t.need_refresh <- true;
    Lwt_condition.broadcast t.cond ()

  let rec enable t =
    t.watch (refresh t) >>= fun unwatch ->
    if t.ref_count = 0 then disable ~unwatch t
    else get_value t ~unwatch
  and disable ~unwatch t =
    unwatch () >>= fun () ->
    if t.ref_count > 0 then enable t
    else (
      assert (t.active);
      t.active <- false;
      (* Clear the saved value, so that if we get activated again then we don't
         start by serving up the previous value, which could be quite stale by then. *)
      t.value <- Error (`Active `Running);
      Lwt.return `Finished
    )
  and get_value ~unwatch t =
    t.need_refresh <- false;
    t.read () >>= fun v ->
    t.value <- (v :> _ Current_term.Output.t);
    Engine.update ();
    wait ~unwatch t
  and wait ~unwatch t =
    if t.ref_count = 0 then disable ~unwatch t
    else if t.need_refresh then get_value ~unwatch t
    else Lwt_condition.wait t.cond >>= fun () -> wait ~unwatch t

  let input t =
    Input.of_fn @@ fun _env ->
    t.ref_count <- t.ref_count + 1;
    Input.register_actions @@ object
      method rebuild = None   (* Might be useful to implement this *)
      method pp f = t.pp f
      method release =
        assert (t.ref_count > 0);
        t.ref_count <- t.ref_count - 1;
        if t.ref_count = 0 then Lwt_condition.broadcast t.cond ()
    end;
    if not t.active then (
      t.active <- true;
      Lwt.async (fun () -> enable t >|= fun `Finished -> ())
    );  (* (else the previous thread will check [ref_count] before exiting) *)
    t.value, None

  let create ~read ~watch ~pp =
    let cond = Lwt_condition.create () in
    {
      ref_count = 0;
      active = false;
      need_refresh = true;
      cond;
      value = Error (`Active `Running);
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
module Job = Job
module Process = Process
module Switch = Switch
module Pool = Pool
module Log_matcher = Log_matcher
