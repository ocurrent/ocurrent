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
  Current.Process.exec ~switch ~job cmd >>!= fun () ->
  Current.Switch.turn_off switch @@ Ok () >>= fun () ->
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
  Current.Process.check_output ~switch ~job cmd >>!= fun out ->
  Current.Switch.turn_off switch @@ Ok () >>= fun () ->
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
  let thread = Current.Process.exec ~switch ~job cmd in
  Current.Switch.turn_off switch @@ Error (`Msg "Timeout") >>= fun () ->
  thread >>= fun res ->
  begin match res with
    | Ok () -> Alcotest.fail "Should have failed!"
    | Error `Msg m when Astring.String.is_prefix ~affix:"Command \"sleep\" \"120\" failed with signal" m -> ()
    | Error `Msg m -> Alcotest.failf "Expected signal error, not %S" m
  end;
  let path = Job.log_path (Job.id job) |> Stdlib.Result.get_ok in
  Alcotest.(check string) "Log" "1970-01-01 00:00.00: Exec: \"sleep\" \"120\"\n\
                                 1970-01-01 00:00.00: Timeout\n" (read path);
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case "streams" `Quick streams;
    Alcotest_lwt.test_case "output" `Quick output;
    Alcotest_lwt.test_case "cancel" `Quick cancel;
  ]
