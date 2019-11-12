open Lwt.Infix

module Job = Current.Job

let read path =
  let ch = open_in_bin (Fpath.to_string path) in
  let data = really_input_string ch (in_channel_length ch) in
  close_in ch;
  data

let ( >>!= ) x f =
  x >>= function
  | Ok y -> f y
  | Error `Msg m -> failwith m

let streams _switch () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"streams" () in
  let config = Current.Config.v () in
  let job = Job.create ~switch ~label:"streams" ~config () in
  let log_data = Job.wait_for_log_data job in
  assert (Lwt.state log_data = Lwt.Sleep);
  let cmd = ("", [| "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" |]) in
  Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->
  Current.Switch.turn_off switch >>= fun () ->
  assert (Lwt.state log_data != Lwt.Sleep);
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Combined results" "1970-01-01 00:00.00: Exec: \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\"\n\
                                              out1\nout2\nout3\n" (read path);
  Lwt.return_unit

let output _switch () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"output" () in
  let config = Current.Config.v () in
  let job = Job.create ~switch ~label:"output" ~config () in
  let cmd = ("", [| "sh"; "-c"; "echo out1; echo >&2 out2; echo out3" |]) in
  Current.Process.check_output ~cancellable:true ~job cmd >>!= fun out ->
  Current.Switch.turn_off switch >>= fun () ->
  Alcotest.(check string) "Output" "out1\nout3\n" out;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"sh\" \"-c\" \"echo out1; echo >&2 out2; echo out3\"\n\
                                 out2\n" (read path);
  Lwt.return_unit

let cancel _switch () =
  Job.timestamp := (fun () -> 0.0);
  let switch = Current.Switch.create ~label:"cancel" () in
  let config = Current.Config.v () in
  let job = Job.create ~switch ~label:"output" ~config () in
  let cmd = ("", [| "sleep"; "120" |]) in
  let thread = Current.Process.exec ~cancellable:true ~job cmd in
  Current.Job.cancel job "Timeout";
  thread >>= fun res ->
  begin match res with
    | Ok () -> Alcotest.fail "Should have failed!"
    | Error `Msg m when Astring.String.is_prefix ~affix:"Command \"sleep\" \"120\" failed with signal" m -> ()
    | Error `Msg m -> Alcotest.failf "Expected signal error, not %S" m
  end;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"sleep\" \"120\"\n\
                                 1970-01-01 00:00.00: Cancelling: Timeout\n" (read path);
  Lwt.return_unit

let pp_lwt_state f = function
  | Lwt.Sleep -> Fmt.string f "Sleep"
  | Lwt.Return () -> Fmt.string f "Returned"
  | Lwt.Fail ex -> Fmt.exn f ex

let lwt_state = Alcotest.testable pp_lwt_state (=)

let pool _switch () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 1 in
  let sw1 = Current.Switch.create ~label:"cancel-1" () in
  let sw2 = Current.Switch.create ~label:"cancel-2" () in
  let job1 = Job.create ~switch:sw1 ~label:"job-1" ~config () in
  let job2 = Job.create ~switch:sw2 ~label:"job-2" ~config () in
  let s1 = Job.start ~pool ~level:Current.Level.Harmless job1 in
  let s2 = Job.start ~pool ~level:Current.Level.Harmless job2 in
  Alcotest.(check lwt_state) "First job started" Lwt.(Return ()) (Lwt.state s1);
  Alcotest.(check lwt_state) "Second job queued" Lwt.Sleep (Lwt.state s2);
  Current.Switch.turn_off sw1 >>= fun () ->
  Alcotest.(check lwt_state) "Second job ready" Lwt.(Return ()) (Lwt.state s2);
  Current.Switch.turn_off sw2

let pool_cancel _switch () =
  let config = Current.Config.v () in
  let pool = Current.Pool.create ~label:"test" 0 in
  let sw1 = Current.Switch.create ~label:"cancel-1" () in
  let job1 = Job.create ~switch:sw1 ~label:"job-1" ~config () in
  let s1 = Job.start ~pool ~level:Current.Level.Harmless job1 in
  Alcotest.(check lwt_state) "Job queued" Lwt.Sleep (Lwt.state s1);
  Current.Job.cancel job1 "Cancel";
  Job.log job1 "Continuing job for a bit";
  Alcotest.(check lwt_state) "Job cancelled" (Lwt.Fail (Failure "Cancelled waiting for resource from pool \"test\"")) (Lwt.state s1);
  Lwt.return_unit

let tests =
  [
    Driver.test_case_gc "streams" streams;
    Driver.test_case_gc "output" output;
    Driver.test_case_gc "cancel" cancel;
    Driver.test_case_gc "pool" pool;
    Driver.test_case_gc "pool_cancel" pool_cancel;
  ]
