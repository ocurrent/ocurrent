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

let input = Current.monitor ~read ~watch ~pp

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)

let wanted = Bool_var.create ~name:"wanted" (Ok true)

let test_pipeline () =
  let* wanted = Bool_var.get wanted in
  if wanted then let* out = Current.track input in Current.fail out
  else Current.return ()

let get_watch () =
  match !w with
  | None -> failwith "No active watch!"
  | Some w -> w

let result =
  let pp f = function
    | `Pending -> Fmt.string f "Pending"
    | (`Msg m) -> Fmt.pf f "ERR: %s" m
  in
  let error = Alcotest.testable pp (=) in
  Alcotest.(result unit) error

let trace step out inputs =
  incr step;
  let step = !step in
  Logs.info (fun f -> f "Step %d (inputs = %a)" step Fmt.(Dump.list Current.Input.pp_watch) inputs);
  match step with
  | 1 ->
    (* Although there is data ready, we shouldn't have started the read yet
       because we're still enabling the watch. *)
    Alcotest.check result "Initially pending" (Error `Pending) out;
    assert (!w <> None);
    assert (List.length inputs = 2);
    let w = get_watch () in
    Lwt.wakeup w.set_ready ()
  | 2 ->
    Alcotest.check result "Read complete" (Error (`Msg "init")) out;
    let w = get_watch () in
    w.update ();  (* Calls read immediately *)
    w.update ();  (* Marks as out-of-date *)
    w.update ();
    data := Some (Ok "foo");
    Lwt_condition.broadcast data_cond ()  (* First read completes *)
  | 3 ->
    Alcotest.check result "Read foo" (Error (`Msg "foo")) out;
    data := Some (Ok "bar");
    Lwt_condition.broadcast data_cond ()  (* Second read completes *)
  | 4 ->
    Alcotest.check result "Read bar" (Error (`Msg "bar")) out;
    Bool_var.set wanted (Ok false)
  | 5 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    assert (List.length inputs = 1);
    assert (!w <> None);
    (* Wanted again, before we've finished shutting down. *)
    Bool_var.set wanted (Ok true)
  | 6 ->
    Alcotest.check result "Shutdown cancelled" (Error (`Msg "bar")) out;
    assert (!w <> None);
    Lwt_condition.broadcast unwatch ();   (* Allow first shutdown to finish *)
    data := Some (Ok "restart");
    Lwt_condition.broadcast data_cond (); (* Will re-read after shutdown *)
    assert (!w <> None);
    let w = get_watch () in
    Lwt.wakeup w.set_ready ()
  | 7 ->
    Alcotest.check result "Read restart" (Error (`Msg "restart")) out;
    Bool_var.set wanted (Ok false)
  | 8 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    assert (List.length inputs = 1);
    assert (!w <> None);
    Lwt_condition.broadcast unwatch ();   (* Allow shutdown to finish *)
    assert (!w = None);
    Bool_var.set wanted (Ok true)
  | 9 ->
    Alcotest.check result "Pending again" (Error `Pending) out;
    let w = get_watch () in
    Lwt.wakeup w.set_ready ();
    data := Some (Ok "baz");
    Lwt_condition.broadcast data_cond ()
  | 10 ->
    Alcotest.check result "Read baz" (Error (`Msg "baz")) out;
    raise Exit
  | _ ->
    assert false


let () =
  let step = ref 0 in
  try Lwt_main.run @@ Current.Engine.run test_pipeline ~trace:(trace step)
  with Exit -> ()
