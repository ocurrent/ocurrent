open Lwt.Infix
open Current.Syntax

let () = Driver.init_logging ()

let data = ref (Some (Ok "init"))
let data_cond = Lwt_condition.create ()
let unwatch = Lwt_condition.create ()

type watch = {
  ready : unit Lwt.t;
  set_ready : unit Lwt.u;
  update : unit -> unit;
}

let w = ref None

let rec read () =
  Logs.info (fun f -> f "read");
  match !data with
  | Some x ->
    data := None;
    Lwt.return x
  | None ->
    Lwt_condition.wait data_cond >>= read

let watch update =
  Logs.info (fun f -> f "Installing watch");
  assert (!w = None);
  let ready, set_ready = Lwt.wait () in
  let watch = { ready; set_ready; update } in
  w := Some watch;
  ready >|= fun () ->
  Logs.info (fun f -> f "Watch installed");
  fun () ->
    Logs.info (fun f -> f "Uninstalling watch");
    Lwt_condition.wait unwatch >|= fun () ->
    w := None;
    Logs.info (fun f -> f "Watch uninstalled")

let pp f = Fmt.string f "watch"

let monitor = Current.Monitor.create ~read ~watch ~pp

let input () =
  Current.component "input" |>
  let> () = Current.return () in
  Current.Monitor.input monitor

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)

let wanted = Bool_var.create ~name:"wanted" (Ok true)

let test_pipeline () =
  let* wanted = Bool_var.get wanted in
  if wanted then let* out = input () in Current.fail out
  else Current.return ()

let get_watch () =
  match !w with
  | None -> failwith "No active watch!"
  | Some w -> w

let result =
  let pp f = function
    | `Active `Ready -> Fmt.string f "Ready"
    | `Active `Running -> Fmt.string f "Running"
    | (`Msg m) -> Fmt.pf f "ERR: %s" m
  in
  let error = Alcotest.testable pp (=) in
  Alcotest.(result unit) error

let trace step ~next:_ { Current.Engine.value = out; _ } =
  incr step;
  let step = !step in
  match step with
  | 1 ->
    (* Although there is data ready, we shouldn't have started the read yet
       because we're still enabling the watch. *)
    Lwt.pause () >>= fun () ->
    Alcotest.check result "Initially pending" (Error (`Active `Running)) out;
    assert (!w <> None);
    let w = get_watch () in
    Lwt.wakeup w.set_ready ();
    Lwt.return_unit
  | 2 ->
    Alcotest.check result "Read complete" (Error (`Msg "init")) out;
    let w = get_watch () in
    w.update ();  (* Calls read immediately *)
    Lwt.pause () >>= fun () ->
    w.update ();  (* Marks as out-of-date *)
    Lwt.pause () >>= fun () ->
    w.update ();
    Lwt.pause () >>= fun () ->
    data := Some (Ok "foo");
    Lwt_condition.broadcast data_cond ();  (* First read completes *)
    Lwt.return_unit
  | 3 ->
    Alcotest.check result "Read foo" (Error (`Msg "foo")) out;
    data := Some (Ok "bar");
    Lwt_condition.broadcast data_cond ();  (* Second read completes *)
    Lwt.return_unit
  | 4 ->
    Alcotest.check result "Read bar" (Error (`Msg "bar")) out;
    Bool_var.set wanted (Ok false);
    Lwt.return_unit
  | 5 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    assert (!w <> None);
    Lwt.pause () >>= fun () ->
    (* Wanted again, before we've finished shutting down. *)
    Bool_var.set wanted (Ok true);
    Lwt.return_unit
  | 6 ->
    Alcotest.check result "Shutdown cancelled" (Error (`Msg "bar")) out;
    assert (!w <> None);
    Lwt_condition.broadcast unwatch ();   (* Allow first shutdown to finish *)
    Lwt.pause () >>= fun () ->
    data := Some (Ok "restart");
    Lwt_condition.broadcast data_cond (); (* Will re-read after shutdown *)
    Lwt.pause () >>= fun () ->
    assert (!w <> None);
    let w = get_watch () in
    Lwt.wakeup w.set_ready ();
    Lwt.return_unit
  | 7 ->
    Alcotest.check result "Read restart" (Error (`Msg "restart")) out;
    Bool_var.set wanted (Ok false);
    Lwt.pause () >>= fun () ->
    Lwt.return_unit
  | 8 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    Lwt.pause () >>= fun () ->
    assert (!w <> None);
    Lwt_condition.broadcast unwatch ();   (* Allow shutdown to finish *)
    Lwt.pause () >>= fun () ->
    assert (!w = None);
    Bool_var.set wanted (Ok true);
    Lwt.return_unit
  | 9 ->
    Alcotest.check result "Pending again" (Error (`Active `Running)) out;
    Lwt.pause () >>= fun () ->
    let w = get_watch () in
    Lwt.wakeup w.set_ready ();
    data := Some (Ok "baz");
    Lwt_condition.broadcast data_cond ();
    Lwt.return_unit
  | 10 ->
    Alcotest.check result "Read baz" (Error (`Msg "baz")) out;
    raise Exit
  | _ ->
    assert false

let basic _switch () =
  let step = ref 0 in
  let engine = Current.Engine.create test_pipeline ~trace:(trace step) in
  Lwt.catch
    (fun () ->Current.Engine.thread engine)
    (function
      | Exit -> Lwt.return_unit
      | ex -> Lwt.fail ex
    )

let tests =
  [
    Driver.test_case_gc "basic" basic;
  ]
