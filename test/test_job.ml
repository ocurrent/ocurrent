module Job = Current.Job

let read path =
  let ch = open_in_bin (Fpath.to_string path) in
  let data = really_input_string ch (in_channel_length ch) in
  close_in ch;
  data

let ( >>!= ) x f =
  x |> function
  | Ok y -> f y
  | Error `Msg m -> failwith m

exception Test_cancel

let with_cancellable_switch fn =
  try
    Eio.Switch.run @@ fun sw ->
    fn (sw, fun () -> Eio.Switch.fail ~bt:(Printexc.get_callstack 5) sw Test_cancel)
  with
    | Test_cancel -> ()

let streams p _sw () =
  Job.timestamp := (fun () -> 0.0);
  let log_data, set_log_data = Eio.Promise.create () in
  let job_ref = ref None in
  let run () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let config = Current.Config.v () in
    let job = Job.create ~sw ~label:"streams" ~config () in
    job_ref := Some job;
    Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve set_log_data @@ Job.wait_for_log_data job);
    assert (not (Eio.Promise.is_resolved log_data));
    let cmd = ("sh", [ "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" ]) in
    Current.Process.exec ~cancellable:true ~job p cmd >>!= fun () ->
    cancel ()
  in
  run ();
  assert (Eio.Promise.is_resolved log_data);
  let path = Job.log_path (Job.id (Option.get !job_ref)) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Combined results" "1970-01-01 00:00.00: Exec: (\"sh\", \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\")\n\
                                              out1\nout2\nout3\n" (read path)

let output p _sw () =
  Job.timestamp := (fun () -> 0.0);
  let job_ref = ref None in
  let out_ref = ref None in
  let run () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let config = Current.Config.v () in
    let job = Job.create ~sw ~label:"output" ~config () in
    job_ref := Some job;
    let cmd = ("sh", [ "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" ]) in
    Current.Process.check_output ~cancellable:true ~job p cmd >>!= fun out ->
    out_ref := Some out;
    cancel ()
  in
  run ();
  Alcotest.(check string) "Output" "out1\nout3\n" (Option.get !out_ref);
  let path = Job.log_path (Job.id (Option.get !job_ref)) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"sh\", \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\")\n\
                                 out2\n" (read path)

let pp_cmd ppf (v, args) =
  let remove_token s =
    match Astring.String.cut ~sep:":" s with
    | Some ("token", _secret) -> "token:<TOKEN>"
    | _ -> s
  in
  Current.Process.pp_cmd ppf (v, List.map remove_token args)

let pp_command p _sw () =
  Job.timestamp := (fun () -> 0.0);
  let job_ref = ref None in
  let out_ref = ref None in
  let run () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let config = Current.Config.v () in
    let job = Job.create ~sw ~label:"output" ~config () in
    job_ref := Some job;
    let cmd = ("echo", [ "echo"; "token:abcdefgh" ]) in
    Current.Process.check_output ~pp_cmd ~cancellable:true ~job p cmd >>!= fun out ->
    out_ref := Some out;
    cancel ()
  in
  run ();
  Alcotest.(check string) "Output" "token:abcdefgh\n" (Option.get !out_ref);
  let path = Job.log_path (Job.id (Option.get !job_ref)) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"echo\", \"echo\" \"token:<TOKEN>\")\n" (read path)

let cancel p _sw () =
  Job.timestamp := (fun () -> 0.0);
  Eio.Switch.run @@ fun sw ->
  let config = Current.Config.v () in
  let job = Job.create ~sw ~label:"output" ~config () in
  let cmd = ("sleep", [ "sleep"; "120" ]) in
  let res = Eio.Fiber.fork_promise ~sw (fun () -> Current.Process.exec ~cancellable:true ~job p cmd) in
  Current.Job.cancel job "Timeout";
  begin match Eio.Promise.await_exn res with
    | Ok () -> Alcotest.fail "Should have failed!"
    (* TODO: I think this should be failed with signal as we're cancelling the exec? *)
    | Error `Msg m when Astring.String.is_prefix ~affix:"Command (\"sleep\", \"sleep\" \"120\") failed with signal" m -> ()
    | Error `Msg m -> Alcotest.failf "Expected signal error, not %S" m
  end;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: (\"sleep\", \"sleep\" \"120\")\n\
                                 1970-01-01 00:00.00: Cancelling: Timeout\n" (read path)

let pp_promise f = function
  | None -> Fmt.string f "Sleep"
  | Some (Ok ()) -> Fmt.string f "Ok ()"
  | Some (Error ex) -> Fmt.exn f ex

let promise_state = Alcotest.testable pp_promise (=)

let pool _sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let run1 () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let job1 = Job.create ~sw ~label:"job-1" ~config () in
    let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
    Alcotest.(check promise_state) "First job started" (Some (Ok ())) (Eio.Promise.peek s1);
    Eio.Fiber.yield ();
    cancel ()
  in
  let run2 () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let job2 = Job.create ~sw ~label:"job-2" ~config () in
    let s2 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job2) in
    Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
    let _r = Eio.Promise.await s2 in
    Alcotest.(check promise_state) "Second job ready" (Some (Ok ())) (Eio.Promise.peek s2);
    cancel ()
  in
  Eio.Fiber.both run1 run2

let pool_cancel sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 0 in
  Eio.Switch.run @@ fun sw1 ->
  let job1 = Job.create ~sw:sw1 ~label:"job-1" ~config () in
  let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
  Alcotest.(check promise_state) "Job queued" None (Eio.Promise.peek s1);
  Current.Job.cancel job1 "Cancel";
  Eio.Fiber.yield ();
  let _r = Eio.Promise.await s1 in
  Job.log job1 "Continuing job for a bit";
  Alcotest.(check promise_state) "Job cancelled" (Some (Error (Failure "Cancelled waiting for resource from pool \"test\""))) (Eio.Promise.peek s1)

let pool_priority _sw () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let run1 () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let job1 = Job.create ~sw ~label:"job-1" ~config () in
    let s1 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job1) in
    Alcotest.(check promise_state) "First job started" (Some (Ok ())) (Eio.Promise.peek s1);
    Eio.Fiber.yield ();
    cancel ()
  in
  let run2 () =
    with_cancellable_switch @@ fun (sw, _cancel) ->
    let job2 = Job.create ~sw ~label:"job-2" ~config () in
    let s2 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job2) in
    Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
    Eio.Fiber.yield ();
    Alcotest.(check promise_state) "Second job queued" None (Eio.Promise.peek s2);
    Eio.Fiber.yield ();
    Eio.Promise.await_exn s2;
    Alcotest.(check promise_state) "Second job ready" (Some (Ok ())) (Eio.Promise.peek s2);
    Eio.Fiber.yield ()
  in
  let run3 () =
    with_cancellable_switch @@ fun (sw, cancel) ->
    let job3 = Job.create ~sw ~priority:`High ~label:"job-3" ~config () in
    let s3 = Eio.Fiber.fork_promise ~sw (fun () -> Job.start ~pool ~level:Current.Level.Harmless job3) in
    Alcotest.(check promise_state) "Third job queued" None (Eio.Promise.peek s3);
    (* TODO: Not sure why yield isn't enough in these tests... Probably just haven't got the same
       Lwt.pause placement as before.  *)
    Eio.Fiber.yield ();
    Eio.Promise.await_exn s3;
    Alcotest.(check promise_state) "High-priority third job ready" (Some (Ok ())) (Eio.Promise.peek s3);
    cancel ()
  in
  Eio.Fiber.all [
    run1;
    run2;
    run3
  ]


let tests ~proc =
  [
    Driver.test_case_gc "streams" (streams proc);
    Driver.test_case_gc "output" (output proc);
    Driver.test_case_gc "pp_cmd" (pp_command proc);
    Driver.test_case_gc "cancel" (cancel proc);
    Driver.test_case_gc "pool" pool;
    Driver.test_case_gc "pool_cancel" pool_cancel;
    Driver.test_case_gc "pool_priority" pool_priority;
  ]
