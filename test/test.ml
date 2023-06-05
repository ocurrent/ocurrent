open Current.Syntax    (* let*, let+, etc *)

let () = Logs.set_level ~all:true (Some (Logs.Debug))

module Git = Current_git_test
module Docker = Current_docker_test
module Opam = Current_opam_test

let () = Driver.init_logging ()

let fetch = Git.fetch
let build = Docker.build
let test = Docker.run ~cmd:["make"; "test"]
let push = Docker.push

let engine_result =
  Alcotest.testable (Current_term.Output.pp Fmt.(const string "()")) (Current_term.Output.equal (=))

let observe_result fmt =
  Alcotest.testable
    (Current_term.Output.Blockable.pp fmt)
    (Current_term.Output.Blockable.equal (=))

let analyse ~lint src =
  Current.component "analyse" |>
  let** _ = src in
  Current.return lint

let lint src ~linter =
  Current.component "lint" |>
  let** _ = src
  and* _ = linter in
  Current.return ()

module Commit_var = Current.Var(Git.Commit)

let test_commit =
  Git.Commit.v ~repo:"my/project" ~hash:"123"

let head = Commit_var.create ~name:"head" (Ok test_commit)

let with_commit v () =
  v (Commit_var.get head)

(* A very simple linear pipeline. Given a commit (e.g. the head of
   a PR on GitHub), this returns success if the tests pass on it. *)
let v1 ~sw commit =
  commit |> fetch ~sw |> build |> test ~sw

let test_v1 sw () =
  Driver.test ~name:"v1" (with_commit (v1 ~sw)) @@ function
  | 1 ->
    Logs.debug (fun f -> f "Done!");
    Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_v1_cancel sw () =
  Driver.test ~name:"v1c" (with_commit (v1 ~sw)) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Driver.cancel "docker run \"image-src-123\" \"make\" \"test\" (in-progress)"
  | _ -> raise Exit

(* Similar, but here the test step requires both the binary and
   the source (perhaps for the test cases). If the tests pass then
   it deploys the binary too. *)
let v2 ~sw commit =
  let src = fetch ~sw commit in
  let bin = build src in
  bin |> Current.gate ~on:(test ~sw bin) |> push ~sw ~tag:"foo/bar"

let test_v2 sw () =
  let config = Current.Config.v ~confirm:Current.Level.Dangerous () in
  Driver.test ~config ~name:"v2" (with_commit (v2 ~sw)) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | 3 -> Current.Config.set_confirm config None
  | _ -> raise Exit

(* Build Linux, Mac and Windows binaries. If *all* tests pass (for
   all platforms) then deploy all binaries. *)
let v3 ~sw commit =
  let platforms = ["lin"; "mac"; "win"] in
  let src = fetch ~sw commit in
  let binaries = List.map (fun p -> p, build ~on:p src) platforms in
  let test (_p, x) = test ~sw x in
  let tests = Current.all @@ List.map test binaries in
  let gated_deploy (p, x) =
    let tag = Fmt.str "foo/%s" p in
    x |> Current.gate ~on:tests |> push ~sw ~tag
  in
  Current.all @@ List.map gated_deploy binaries

let test_v3 sw () =
  let final_stats =
    { Current_term.S.
      ok = 8;
      failed = 1;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 1;
      blocked = 3;
    }
  in
  Driver.test ~name:"v3" (with_commit (v3 ~sw)) ~final_stats @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 ->
    Docker.complete "lin-image-src-123" ~cmd:["make"; "test"] @@ Ok ();
    Docker.complete "win-image-src-123" ~cmd:["make"; "test"] @@ Error (`Msg "Missing DLL");
  | _ -> raise Exit

(* Monadic bind is also available if you need to take a decision based
   on the actual source code before deciding on the rest of the pipeline.
   The let** form allows you to name the box.
   The static analysis will only show what happens up to this step until
   it actually runs, after which it will show the whole pipeline. *)
let v4 ~sw commit =
  let src = fetch ~sw commit in
  Current.component "custom-build" |>
  let** src = src in
  if Fpath.to_string src = "src-123" then build (Current.return src) |> test ~sw
  else Current.fail "Wrong hash!"

let test_v4 sw () =
  Driver.test ~name:"v4" (with_commit (v4 ~sw)) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Error (`Msg "Failed")
  | _ -> raise Exit

(* The opam-repo-ci pipeline. Build the package and test it.
   If the tests pass then query for the rev-deps and build and
   test each of them. Using [list_iter] here instead of a bind
   allows us to see the whole pipeline statically, before we've
   actually calculated the rev-deps. *)
let v5 ~sw commit =
  let src = fetch ~sw commit in
  let bin = build src in
  let ok = test ~sw bin in
  Opam.revdeps src
  |> Current.gate ~on:ok
  |> Current.list_iter (module Git.Commit) (fun s -> s |> fetch ~sw |> build |> test ~sw)

let test_v5 sw () =
  let final_stats =
    { Current_term.S.
      ok = 7;
      failed = 0;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 2;
      blocked = 4;
    }
  in
  Driver.test ~name:"v5" ~final_stats (with_commit (v5 ~sw)) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_v5_nil sw () =
  let final_stats =
    { Current_term.S.
      ok = 7;
      failed = 0;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 0;
      blocked = 3;
    }
  in
  let test_commit = Git.Commit.v ~repo:"my/project" ~hash:"456" in
  Driver.test ~name:"v5n" ~final_stats (with_commit (v5 ~sw)) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-456" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_option ~sw ~case commit =
  let src = fetch ~sw commit in
  analyse ~lint:case src
  |> Current.option_map (fun linter -> lint src ~linter)
  |> Current.ignore_value

let test_option_some sw () =
  let final_stats =
    { Current_term.S.
      ok = 6;
      failed = 0;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 0;
      blocked = 0;
    }
  in
  test_option ~sw ~case:(Some "ocamlformat")
  |> with_commit
  |> (fun c -> Driver.test ~final_stats ~name:"option-some" c @@ function
  | 1 -> Git.complete_clone test_commit
  | _ -> raise Exit)

let test_option_none sw () =
  let final_stats =
    { Current_term.S.
      ok = 5;
      failed = 0;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 0;
      blocked = 1;
    }
  in
  test_option ~sw ~case:None
  |> with_commit
  |> (fun c -> Driver.test ~final_stats ~name:"option-none" c @@ function
    | 1 -> Git.complete_clone test_commit
    | _ -> raise Exit)

(* This is just to check the diagram when the state box is hidden. *)
let test_state _sw () =
  let pipeline () =
    Current.component "set-status" |>
    let** value = Current.state ~hidden:true (Current.active `Ready) in
    Alcotest.(check engine_result) "Pending" (Error (`Active `Ready)) value;
    Current.return ()
  in
  Driver.test ~name:"state" pipeline @@ function
  | _ -> raise Exit

let test_pair _switch () =
  let show label x = (* Make it show up on the diagram so we can see the input state. *)
    Current.component "%s" label |>
    let> () = x in
    Current.Primitive.const ()
  in
  let check name expected x =
    let+ s = Current.state (show name x) in
    Alcotest.check engine_result name expected s
  in
  let pipeline () =
    let ok = Current.return () in
    let pending = Current.active `Running in
    let failed = Current.fail "failed" in
    Current.all [
      check "Blocked-1" (Error (`Msg "failed")) (Current.all [ok; pending; failed]);
      check "Blocked-2" (Error (`Msg "failed")) (Current.all [failed; pending; ok]);
      check "Blocked-3" (Error (`Active `Running)) (Current.all [pending; ok]);
    ]
  in
  Driver.test ~name:"pair" pipeline (fun _ -> raise Exit)
    ~final_stats:
    { Current_term.S.
      ok = 2;
      failed = 0;
      waiting_for_confirmation = 0;
      ready = 0;
      running = 0;
      blocked = 3;
    }

(* This is just to check the diagram. *)
let test_context _switch () =
  let label l =
    Current.component "%s" l |>
    let> () = Current.return () in
    Current.Primitive.const ()
  in
  let a = label "a" in
  let b = label "b" in
  let pipeline () =
    Current.with_context a @@ fun () ->
    Current.with_context b @@ fun () ->
    label "c"
  in
  Driver.test ~name:"context" pipeline @@ function
  | _ -> raise Exit

let test_with base src =
  Current.component "test" |>
  let> _base = base
  and> _src = src in
  Current.Primitive.const ()

let latch ~sw commit =
  let base = Docker.pull ~sw "alpine" in
  let src = fetch ~sw commit in
  test_with base src

let test_latch sw () =
  Driver.test ~name:"latch" (with_commit (latch ~sw)) @@ function
  | 1 ->
    (* The "docker pull" box is orange as the image isn't available yet *)
    Git.complete_clone test_commit;
    Docker.complete_pull "alpine" @@ Ok "alpine:3.10";
  | 2 ->
    (* The "docker pull" box is green as the image has arrived *)
    Docker.update_pull "alpine";
  | 3 ->
    (* The "docker pull" box shows an orange-to-green gradient to indicate a
       background update, while "test" remains green (using the previous image). *)
    Docker.complete_pull "alpine" @@ Ok "alpine:3.11";
  | _ ->
    (* The "docker pull" box is green again *)
    raise Exit

module Term = Current_term.Make(String)

let test_all_labelled () =
  let test x = Current_incr.observe (Term.Executor.run (Term.all_labelled x)) in
  Alcotest.check engine_result "all_ok" (Ok ()) @@ test [
    "Alpine", Term.return ();
    "Debian", Term.return ();
  ];
  Alcotest.check engine_result "1st fails" (Error (`Msg "Alpine failed: apk")) @@ test [
    "Alpine", Term.fail "apk";
    "Debian", Term.return ();
  ];
  Alcotest.check engine_result "2nd fails" (Error (`Msg "Debian failed: apt")) @@ test [
    "Alpine", Term.return ();
    "Debian", Term.fail "apt";
  ];
  Alcotest.check engine_result "different failures" (Error (`Msg "Alpine, Debian failed")) @@ test [
    "Alpine", Term.fail "apk";
    "Debian", Term.fail "apt";
  ];
  Alcotest.check engine_result "same failure" (Error (`Msg "Alpine, Debian failed: ENOSPACE")) @@ test [
    "Alpine", Term.fail "ENOSPACE";
    "Debian", Term.fail "ENOSPACE";
  ]

let job x =
  let info = Term.component "job" in
  Term.primitive ~info (fun () -> Current_incr.const (Ok (), Some x)) @@ Term.return ()

let test_metadata () =
  let pipeline =
    let j = Term.map ignore (job "1") in
    Term.Analysis.metadata j
  in
  let job_id = Current_incr.observe (Term.Executor.run pipeline) in
  Alcotest.(check (result (option string) reject)) "Got job ID" (Ok (Some "1")) job_id

let test_observe _switch () =
  let ok = Current.return "a" in
  let failure = Current.fail "oh no" in
  let active = Current.active `Running in
  let blocked =
    let+ v = failure in
    v ^ "::"
  in
  let pipeline () =
    let+ _ = ok
    and+ _ = failure
    and+ _ = active
    and+ _ = blocked
    in
    ()
  in
  Driver.test ~name:"observe" pipeline @@ function
  | _ ->
    let observe_result = observe_result Fmt.string in
    Alcotest.(check observe_result) "OK" (Ok "a") (Current.observe ok);
    Alcotest.(check observe_result)
      "Failure" (Error (`Msg "oh no")) (Current.observe failure);
    Alcotest.(check observe_result)
      "Active" (Error (`Active `Running)) (Current.observe active);
    Alcotest.(check observe_result)
      "Blocked" (Error (`Blocked)) (Current.observe blocked);
    raise Exit

let () =
  Eio_main.run @@ fun env ->
  let proc = (Eio.Stdenv.process_mgr env :> Eio.Process.mgr) in
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token ->
    Alcotest.run "test" [
      "pipelines", [
        Driver.test_case_gc "v1"          test_v1;
        Driver.test_case_gc "v1-cancel"   test_v1_cancel;
        Driver.test_case_gc "v2"          test_v2;
        Driver.test_case_gc "v3"          test_v3;
        Driver.test_case_gc "v4"          test_v4;
        Driver.test_case_gc "v5"          test_v5;
        Driver.test_case_gc "v5-nil"      test_v5_nil;
        Driver.test_case_gc "option-some" test_option_some;
        Driver.test_case_gc "option-none" test_option_none;
        Driver.test_case_gc "state"       test_state;
        Driver.test_case_gc "pair"        test_pair;
        Driver.test_case_gc "latch"       test_latch;
        Driver.test_case_gc "context"     test_context;
      ];
      "terms", [
        Alcotest.test_case  "all_labelled" `Quick test_all_labelled;
        Alcotest.test_case  "metadata"     `Quick test_metadata;
        Driver.test_case_gc "observe"             test_observe;
      ];
      "cache", Test_cache.tests;
      "monitor", Test_monitor.tests;
      "job", Test_job.tests ~proc;
      "log_matcher", Test_log_matcher.tests;
    ]
