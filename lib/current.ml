open Lwt.Infix

type 'a or_error = ('a, [`Msg of string]) result

module Config = Config

module Job_map = Map.Make(String)

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "core"

  let evaluations_total =
    let help = "Total number of evaluations" in
    Counter.v ~help ~namespace ~subsystem "evaluations_total"
end

type job_id = string

class type actions = object
  method pp : Format.formatter -> unit
  method cancel : (unit -> unit) option
  method rebuild : (unit -> job_id) option
  method release : unit
end

type job_metadata = {
  job_id : job_id option;
  changed : unit Lwt.t option;
  actions : actions;
}

module Step = struct
  type id = < >

  type t = {
    id : id;
    config : Config.t;
    mutable jobs : actions Job_map.t;
    mutable watches : job_metadata list;
  }

  let id t = t.id

  let create config =
    let jobs = Job_map.empty in
    { config; jobs; watches = []; id = object end }

  let register_job t id actions =
    t.jobs <- Job_map.add id actions t.jobs

  let config t = t.config
end

module Input = struct
  type nonrec job_id = job_id

  type metadata = job_metadata

  type 'a t = Step.t -> 'a Current_term.Output.t * metadata

  type env = Step.t

  let metadata ?job_id ?changed actions =
    { job_id; changed; actions }

  let of_fn f step =
    try f step
    with ex ->
      Log.warn (fun f -> f "Uncaught exception from input: %a" Fmt.exn ex);
      Error (`Msg (Printexc.to_string ex)), metadata @@ object
        method pp f = Fmt.exn f ex
        method cancel = None
        method rebuild = None
        method release = ()
      end

  let const x =
    of_fn @@ fun _env ->
    Ok x, metadata @@ object
      method pp f = Fmt.string f "Input.const"
      method cancel = None
      method rebuild = None
      method release = ()
    end

  let get step (t : 'a t) =
    let value, metadata = t step in
    let { job_id; changed = _; actions } = metadata in
    step.watches <- metadata :: step.watches;
    begin match job_id with
      | None -> ()
      | Some job_id -> Step.register_job step job_id actions
    end;
    (value, job_id)
end

include Current_term.Make(Input)

type 'a term = 'a t

module Var (T : Current_term.S.T) = struct
  type t = {
    mutable current : T.t Current_term.Output.t;
    name : string;
    cond : unit Lwt_condition.t;
  }

  let create ~name current =
    { current; name; cond = Lwt_condition.create () }

  let get t =
    let open Syntax in
    component "%s" t.name |>
    let> () = return () in
    Input.of_fn @@ fun _env ->
    let changed = Lwt_condition.wait t.cond in
    t.current, Input.metadata ~changed @@
    object
      method pp f = Fmt.string f t.name
      method cancel = None
      method rebuild = None
      method release = ()
    end

  let set t v =
    t.current <- v;
    Lwt_condition.broadcast t.cond ()

  let update t f =
    t.current <- f t.current;
    Lwt_condition.broadcast t.cond ()
end

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

module Engine = struct
  type metadata = job_metadata

  type results = {
    value : unit Current_term.Output.t;
    analysis : Analysis.t;
    watches : metadata list;
    jobs : actions Job_map.t;
  }

  type t = {
    thread : 'a. 'a Lwt.t;
    last_result : results ref;
    config : Config.t;
  }

  let booting = {
    value = Error (`Active `Running);
    analysis = Analysis.booting;
    watches = [];
    jobs = Job_map.empty;
  }

  let default_trace r =
    Log.info (fun f -> f "Result: %a" Current_term.(Output.pp Fmt.(unit "()")) r.value);
    Lwt.return_unit

  let create ?(config=Config.default) ?(trace=default_trace) f =
    let last_result = ref booting in
    let rec aux () =
      let old = !last_result in
      Log.debug (fun f -> f "Evaluating...");
      let step = Step.create config in
      let r, an = Executor.run ~env:step f in
      Prometheus.Counter.inc_one Metrics.evaluations_total;
      let watches = step.Step.watches in
      List.iter (fun w -> w.actions#release) old.watches;
      last_result := {
        value = r;
        analysis = an;
        watches;
        jobs = step.Step.jobs;
      };
      trace !last_result >>= fun () ->
      Log.debug (fun f -> f "Waiting for inputs to change...");
      Lwt.choose (filter_map (fun w -> w.changed) watches) >>= fun () ->
      aux ()
    in
    let thread =
      (* The pause lets us start the web-server before the first evaluation,
         and also frees us from handling an initial exception specially. *)
      Lwt.pause () >>= aux
    in
    { thread; last_result; config }

  let state t = !(t.last_result)

  let jobs s = s.jobs

  let config t = t.config

  let thread t = t.thread

  let actions m = m.actions

  let job_id m = m.job_id

  let pp_metadata f m = m.actions#pp f

  let is_stale m =
    match m.changed with
    | Some p -> Lwt.state p <> Lwt.Sleep
    | None -> false
end

module Monitor : sig
  val create :
    read:(unit -> 'a or_error Lwt.t) ->
    watch:((unit -> unit) -> (unit -> unit Lwt.t) Lwt.t) ->
    pp:(Format.formatter -> unit) ->
    'a Input.t
end = struct
  type 'a t = {
    read : unit -> 'a or_error Lwt.t;
    watch : (unit -> unit) -> (unit -> unit Lwt.t) Lwt.t;
    pp : Format.formatter -> unit;
    mutable value : 'a Current_term.Output.t;
    mutable ref_count : int;              (* Number of terms using this input *)
    mutable need_refresh : bool;          (* Update detected after current read started *)
    mutable active : bool;                (* Monitor thread is running *)
    cond : unit Lwt_condition.t;          (* Maybe time to leave the "wait" state *)
    external_cond : unit Lwt_condition.t; (* New value ready for external user *)
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
    Lwt_condition.broadcast t.external_cond ();
    wait ~unwatch t
  and wait ~unwatch t =
    if t.ref_count = 0 then disable ~unwatch t
    else if t.need_refresh then get_value ~unwatch t
    else Lwt_condition.wait t.cond >>= fun () -> wait ~unwatch t

  let run t =
    Input.of_fn @@ fun _env ->
    t.ref_count <- t.ref_count + 1;
    if not t.active then (
      t.active <- true;
      Lwt.async (fun () -> enable t >|= fun `Finished -> ())
    );  (* (else the previous thread will check [ref_count] before exiting) *)
    let changed = Lwt_condition.wait t.external_cond in
    t.value, Input.metadata ~changed @@
    object
      method cancel = None
      method rebuild = None   (* Might be useful to implement this *)
      method pp f = t.pp f
      method release =
        assert (t.ref_count > 0);
        t.ref_count <- t.ref_count - 1;
        if t.ref_count = 0 then Lwt_condition.broadcast t.cond ()
    end

  let create ~read ~watch ~pp =
    let cond = Lwt_condition.create () in
    let external_cond = Lwt_condition.create () in
    let t = {
      ref_count = 0;
      active = false;
      need_refresh = true;
      cond; external_cond;
      value = Error (`Active `Running);
      read; watch; pp
    } in
    run t
end

let monitor = Monitor.create

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
