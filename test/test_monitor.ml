open Current.Syntax

let () = Driver.init_logging ()

let data = ref (Some (Ok "init"))
let data_cond = Eio.Condition.create ()
let unwatch = Eio.Condition.create ()

type watch = {
  set_ready : unit Eio.Promise.u;
  update : unit -> unit;
}

let w = ref None

let rec read () =
  Logs.info (fun f -> f "read");
  match !data with
  | Some x ->
    data := None;
    Eio.Promise.create_resolved x
  | None ->
    Eio.Condition.await_no_mutex data_cond;
    read ()

let watch update =
  Logs.info (fun f -> f "Installing watch");
  assert (!w = None);
  let ready, set_ready = Eio.Promise.create () in
  let watch = { set_ready; update } in
  w := Some watch;
  Eio.Promise.await ready;
  Logs.info (fun f -> f "Watch installed");
  fun () ->
    Logs.info (fun f -> f "Uninstalling watch");
    Eio.Condition.await_no_mutex unwatch;
    Logs.debug (fun f -> f "Setting to none");
    w := None;
    Logs.info (fun f -> f "Watch uninstalled")

let pp f = Fmt.string f "watch"

let input monitor =
  Current.component "input" |>
  let> () = Current.return () in
  Logs.debug (fun f ->f "INPUT");
  Current.Monitor.get monitor

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)

let wanted = Bool_var.create ~name:"wanted" (Ok true)

let test_pipeline sw () =
  let monitor = Current.Monitor.create ~sw ~read ~watch ~pp in
  let* wanted = Bool_var.get wanted in
  if wanted then let* out = input monitor in Current.fail out
  else Current.return ()

let get_watch () =
  match !w with
  | None -> failwith "No active watch!"
  | Some w -> w

let result =
  let pp f = function
    | `Active `Waiting_for_confirmation -> Fmt.string f "Waiting for confirmation"
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
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    Alcotest.check result "Initially pending" (Error (`Active `Running)) out;
    assert (!w <> None);
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ()
  | 2 ->
    Alcotest.check result "Read complete" (Error (`Msg "init")) out;
    let w = get_watch () in
    w.update ();  (* Calls read immediately *)
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    w.update ();  (* Marks as out-of-date *)
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    w.update ();
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    data := Some (Ok "foo");
    Eio.Condition.broadcast data_cond  (* First read completes *)
  | 3 ->
    Alcotest.check result "Read foo" (Error (`Msg "foo")) out;
    data := Some (Ok "bar");
    Eio.Condition.broadcast data_cond  (* Second read completes *)
  | 4 ->
    Alcotest.check result "Read bar" (Error (`Msg "bar")) out;
    Bool_var.set wanted (Ok false)
  | 5 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    assert (!w <> None);
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    (* Wanted again, before we've finished shutting down. *)
    Bool_var.set wanted (Ok true)
  | 6 ->
    Alcotest.check result "Shutdown cancelled" (Error (`Msg "bar")) out;
    assert (!w <> None);
    Eio.Condition.broadcast unwatch;   (* Allow first shutdown to finish *)
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    data := Some (Ok "restart");
    Eio.Condition.broadcast data_cond; (* Will re-read after shutdown *)
    (* Lwt.pause () >>= fun () -> *)
    assert (!w <> None);
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ()
  | 7 ->
    Alcotest.check result "Read restart" (Error (`Msg "restart")) out;
    Bool_var.set wanted (Ok false);
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ()
  | 8 ->
    Alcotest.check result "Not wanted" (Ok ()) out;
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    assert (!w <> None);
    Eio.Condition.broadcast unwatch;   (* Allow shutdown to finish *)
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    assert (!w = None);
    Bool_var.set wanted (Ok true)
  | 9 ->
    Alcotest.check result "Pending again" (Error (`Active `Running)) out;
    (* Lwt.pause () >>= fun () -> *)
    Eio.Fiber.yield ();
    let w = get_watch () in
    Eio.Promise.resolve w.set_ready ();
    data := Some (Ok "baz");
    Eio.Condition.broadcast data_cond
  | 10 ->
    Alcotest.check result "Read baz" (Error (`Msg "baz")) out;
    raise Exit
  | _ ->
    assert false

let basic sw () =
  (let step = ref 0 in
  let engine = Current.Engine.create (test_pipeline sw) ~sw ~trace:(trace step) in
  try Eio.Promise.await_exn @@ Current.Engine.thread engine with
    | Exit -> ()
    | ex -> raise ex); Logs.info (fun f -> f "Do111ne")

let tests =
  [
    Driver.test_case_gc "basic" basic;
  ]
