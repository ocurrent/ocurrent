open Lwt.Infix

type 'a or_error = ('a, [`Msg of string]) result

module Config = struct
  type t = {
    mutable confirm : Level.t option;
    level_cond : unit Lwt_condition.t;
  }

  let v ?confirm () =
    let level_cond = Lwt_condition.create () in
    { confirm; level_cond }

  let default = v ()

  let set_confirm t level =
    t.confirm <- level;
    Lwt_condition.broadcast t.level_cond ()

  let rec confirmed l t =
    match t.confirm with
    | Some threshold when Level.compare l threshold >= 0 ->
      Lwt_condition.wait t.level_cond >>= fun () ->
      confirmed l t
    | _ ->
      Lwt.return_unit

  open Cmdliner

  let cmdliner_confirm =
    let levels = List.map (fun l -> Level.to_string l, Some l) Level.values in
    let conv = Arg.enum @@ ("none", None) :: levels in
    Arg.opt conv None @@
    Arg.info ~doc:"Confirm before starting operations at or above this level."
      ["confirm"]

  let cmdliner =
    let make confirm = v ?confirm () in
    Term.(const make $ Arg.value cmdliner_confirm)
end

module Input = struct
  class type watch = object
    method pp : Format.formatter -> unit
    method changed : unit Lwt.t
    method cancel : (unit -> unit) option
    method release : unit
  end

  type 'a t = unit -> 'a Current_term.Output.t * watch list

  type env = Config.t

  let of_fn t = t

  let get (t : 'a t) = t ()

  let pp_watch f t = t#pp f
end

include Current_term.Make(Input)

let confirmed l =
  let open Syntax in
  let+ config = env in
  Config.confirmed l config

type 'a term = 'a t

module Var (T : Current_term.S.T) = struct
  type t = {
    mutable current : T.t Current_term.Output.t;
    name : string;
    cond : unit Lwt_condition.t;
  }

  let create ~name current =
    { current; name; cond = Lwt_condition.create () }

  let watch t =
    let v = t.current in
    object
      method pp f = Fmt.string f t.name

      method changed =
        let rec aux () =
          if Current_term.Output.equal T.equal t.current v then
            Lwt_condition.wait t.cond >>= aux
          else
            Lwt.return ()
        in aux ()

      method release = ()

      method cancel = None
    end

  let get t =
    track (fun () -> t.current, [watch t] )

  let set t v =
    t.current <- v;
    Lwt_condition.broadcast t.cond ()

  let update t f =
    t.current <- f t.current;
    Lwt_condition.broadcast t.cond ()
end

let default_trace r inputs =
  Log.info (fun f ->
      f "@[<v2>Evaluation complete:@,\
         Result: %a@,\
         Watching: %a@]"
        Current_term.(Output.pp Fmt.(unit "()")) r
        Fmt.(Dump.list Input.pp_watch) inputs
    );
  Lwt.return_unit

module Engine = struct
  type results = {
    value : unit Current_term.Output.t;
    analysis : Analysis.t;
    watches : Input.watch list;
  }

  type t = {
    thread : 'a. 'a Lwt.t;
    last_result : results ref;
  }

  let booting = {
    value = Error (`Pending);
    analysis = Analysis.booting;
    watches = [];
  }

  let create ?(config=Config.default) ?(trace=default_trace) f =
    let last_result = ref booting in
    let rec aux () =
      let old = !last_result in
      Log.info (fun f -> f "Evaluating...");
      let r, an, watches = Executor.run ~env:config f in
      List.iter (fun w -> w#release) old.watches;
      last_result := {
        value = r;
        analysis = an;
        watches;
      };
      trace r watches >>= fun () ->
      Log.info (fun f -> f "Waiting for inputs to change...");
      Lwt.choose (List.map (fun w -> w#changed) watches) >>= fun () ->
      aux ()
    in
    let thread =
      (* The pause lets us start the web-server before the first evaluation,
         and also frees us from handling an initial exception specially. *)
      Lwt.pause () >>= aux
    in
    { thread; last_result }

  let state t = !(t.last_result)

  let thread t = t.thread
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
      t.value <- Error `Pending;
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
    Input.of_fn @@ fun () ->
    t.ref_count <- t.ref_count + 1;
    if not t.active then (
      t.active <- true;
      Lwt.async (fun () -> enable t >|= fun `Finished -> ())
    );  (* (else the previous thread will check [ref_count] before exiting) *)
    let changed = Lwt_condition.wait t.external_cond in
    let watch =
      object
        method changed = changed
        method cancel = None
        method pp f = t.pp f
        method release =
          assert (t.ref_count > 0);
          t.ref_count <- t.ref_count - 1;
          if t.ref_count = 0 then Lwt_condition.broadcast t.cond ()
      end
    in
    t.value, [watch]

  let create ~read ~watch ~pp =
    let cond = Lwt_condition.create () in
    let external_cond = Lwt_condition.create () in
    let t = {
      ref_count = 0;
      active = false;
      need_refresh = true;
      cond; external_cond;
      value = Error `Pending;
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
  let equal () () = true
  let marshal () = "()"
  let unmarshal = function
    | "()" -> ()
    | x -> Fmt.failwith "Unit.unmarshal(%S)" x
end

let db = Disk_store.db
let state_dir = Disk_store.state_dir

module Job = Job
module Process = Process
