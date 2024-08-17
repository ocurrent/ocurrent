open Lwt.Infix
open Lwt.Syntax

module Retry = Current.Retry

let attempt
    (ok : 'ok Alcotest.testable)
    (retry : 'retry Alcotest.testable)
    (fatal : 'fatal Alcotest.testable)
  : ('ok, 'retry, 'fatal) Retry.attempt Alcotest.testable
  =
  let pp = Retry.pp_error
      ~retry:(Alcotest.pp retry)
      ~fatal:(Alcotest.pp fatal)
  in
  let eq = Retry.equal_error
      ~retry:(Alcotest.equal retry)
      ~fatal:(Alcotest.equal fatal)
  in
  let error = Alcotest.testable pp eq in
  Alcotest.result ok error

let err_msg : [`Msg of string] Alcotest.testable =
  let pp fmt (`Msg m) = Fmt.pf fmt "`Msg %a" Fmt.string m in
  let eq (`Msg a) (`Msg b) = String.equal a b in
  Alcotest.testable pp eq

let test_success_without_retry _switch () =
  let strm =
    Retry.on_error (fun () -> Lwt.return_ok 42)
  in

  let msg = "expected success" in
  let expected = (Ok 42) in
  let* actual = Lwt_stream.next strm in
  Alcotest.(check' (attempt int unit unit)) ~msg ~expected ~actual;

  let msg = "expected stream to be empty" in
  let expected = true in
  let+ actual = Lwt_stream.is_empty strm in
  Alcotest.(check' bool) ~msg ~expected ~actual

let test_retries _switch () =
  let strm =
    Retry.on_error (fun () -> Lwt.return_error (`Retry ()))
  in

  let msg = "expected 3 retry errors" in
  let expected = List.init 3 (fun _ -> Error (`Retry ())) in
  let+ actual = Lwt_stream.nget 3 strm in
  Alcotest.(check' (list (attempt unit unit unit))) ~msg ~expected ~actual

let test_retries_before_fatal_error _switch () =
  let retries_before_fatal = 3 in
  let i = ref 0 in
  let strm = Retry.on_error
      (fun () ->
         if !i < retries_before_fatal then (
           incr i;
           Lwt.return_error (`Retry ())
         ) else
           Lwt.return_error (`Fatal ()))
  in

  let msg = "expected 3 retry errors" in
  let expected = retries_before_fatal in
  let* actual = Lwt_stream.nget retries_before_fatal strm >|= List.length in
  Alcotest.(check' int) ~msg ~expected ~actual;

  let msg = "expected fatal error" in
  let expected = Error (`Fatal ()) in
  let* actual = Lwt_stream.next strm in
  Alcotest.(check' (attempt unit unit unit)) ~msg ~expected ~actual;

  let msg = "expected stream to be empty" in
  let+ stream_is_empty = Lwt_stream.is_empty strm in
  Alcotest.(check' bool) ~msg ~expected:true ~actual:stream_is_empty

let test_retries_before_success _switch () =
  let retries_before_fatal = 3 in
  let i = ref 0 in
  let strm = Retry.on_error (fun () ->
      if !i < retries_before_fatal then (
        incr i;
        Lwt.return_error (`Retry ())
      ) else
        Lwt.return_ok ()
    )
  in

  let msg = "expected 3 retry errors" in
  let expected = retries_before_fatal in
  let* actual = Lwt_stream.nget retries_before_fatal strm >|= List.length in
  Alcotest.(check' int) ~msg ~expected ~actual;

  let msg = "expected success error" in
  let expected = Ok () in
  let* actual = Lwt_stream.next strm in
  Alcotest.(check' (attempt unit unit unit)) ~msg ~expected ~actual;

  let msg = "expected stream to be empty" in
  let+ stream_is_empty = Lwt_stream.is_empty strm in
  Alcotest.(check' bool) ~msg ~expected:true ~actual:stream_is_empty

let test_n_times_fatal _switch () =
  let i = ref 0 in
  let operation () =
    if !i < 3 then (
      incr i;
      Lwt.return_error (`Retry ())
    ) else
      Lwt.return_error (`Fatal ())
  in
  let msg = "expected fatal error message" in
  let expected = `Msg "fatal error '<opaque>' (fatal)" in
  let+ actual = Retry.(operation |> on_error |> n_times 5) >|= Result.get_error in
  Alcotest.(check' err_msg) ~msg ~expected ~actual

let test_n_times_exhaustion _switch () =
  let operation () = Lwt.return_error (`Retry ()) in
  let msg = "expected exhaustion error message" in
  let expected = `Msg "retryable error '<opaque>' (exhausted)" in
  let+ actual = Retry.(operation |> on_error |> n_times 5) >|= Result.get_error in
  Alcotest.(check' err_msg) ~msg ~expected ~actual

let test_n_times_success _switch () =
  let i = ref 0 in
  let operation () =
    if !i < 3 then (
      incr i;
      Lwt.return_error (`Retry ())
    ) else
      Lwt.return_ok ()
  in
  try Retry.(operation |> on_error |> n_times 5) >|= Result.get_ok
  with Invalid_argument _ -> Alcotest.fail "expected Ok result"

(* test that the sleeps actually do throttle the computations *)
let test_with_sleep _switch () =
  let duration _ = 0.1 in
  let racing_operation = Lwt_unix.sleep (duration ()) >|= Result.ok in
  let operation () = Lwt.return_error (`Retry ()) in
  let retries = Retry.(operation |> on_error |> with_sleep ~duration |> n_times 5) in
  (* If [with_sleep] is removed the test fails, as expected *)
  let msg = "expected racing_operation to complete before the retries with sleeps" in
  let expected = Ok () in
  let+ actual = Lwt.choose [racing_operation; retries] in
  Alcotest.(check' (result unit err_msg)) ~msg ~expected ~actual

let tests =
  [
    Alcotest_lwt.test_case "test_success_without_retry" `Quick test_success_without_retry;
    Alcotest_lwt.test_case "test_retries" `Quick test_retries;
    Alcotest_lwt.test_case "test_retries_before_fatal_error" `Quick test_retries_before_fatal_error;
    Alcotest_lwt.test_case "test_retries_before_success" `Quick test_retries_before_success;
    Alcotest_lwt.test_case "test_n_times_fatal" `Quick test_n_times_fatal;
    Alcotest_lwt.test_case "test_n_times_exhaustion" `Quick test_n_times_exhaustion;
    Alcotest_lwt.test_case "test_n_times_success" `Quick test_n_times_success;
    Alcotest_lwt.test_case "test_with_sleep" `Quick test_with_sleep;
  ]
