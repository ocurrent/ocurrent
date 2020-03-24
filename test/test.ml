open Current.Syntax    (* let*, let+, etc *)

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
let v1 commit =
  commit |> fetch |> build |> test

let test_v1 _switch () =
  Driver.test ~name:"v1" (with_commit v1) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_v1_cancel _switch () =
  Driver.test ~name:"v1c" (with_commit v1) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Driver.cancel "docker run \"image-src-123\" \"make\" \"test\" (in-progress)"
  | _ -> raise Exit

(* Similar, but here the test step requires both the binary and
   the source (perhaps for the test cases). If the tests pass then
   it deploys the binary too. *)
let v2 commit =
  let src = fetch commit in
  let bin = build src in
  bin |> Current.gate ~on:(test bin) |> push ~tag:"foo/bar"

let test_v2 _switch () =
  let config = Current.Config.v ~confirm:Current.Level.Dangerous () in
  Driver.test ~config ~name:"v2" (with_commit v2) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | 3 -> Current.Config.set_confirm config None
  | _ -> raise Exit

(* Build Linux, Mac and Windows binaries. If *all* tests pass (for
   all platforms) then deploy all binaries. *)
let v3 commit =
  let platforms = ["lin"; "mac"; "win"] in
  let src = fetch commit in
  let binaries = List.map (fun p -> p, build ~on:p src) platforms in
  let test (_p, x) = test x in
  let tests = Current.all @@ List.map test binaries in
  let gated_deploy (p, x) =
    let tag = Fmt.strf "foo/%s" p in
    x |> Current.gate ~on:tests |> push ~tag
  in
  Current.all @@ List.map gated_deploy binaries

let test_v3 _switch () =
  let final_stats =
    { Current_term.S.
      ok = 8;
      failed = 1;
      ready = 0;
      running = 1;
      blocked = 6;
    }
  in
  Driver.test ~name:"v3" (with_commit v3) ~final_stats @@ function
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
let v4 commit =
  let src = fetch commit in
  Current.component "custom-build" |>
  let** src = src in
  if Fpath.to_string src = "src-123" then build (Current.return src) |> test
  else Current.fail "Wrong hash!"

let test_v4 _switch () =
  Driver.test ~name:"v4" (with_commit v4) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Error (`Msg "Failed")
  | _ -> raise Exit

(* The opam-repo-ci pipeline. Build the package and test it.
   If the tests pass then query for the rev-deps and build and
   test each of them. Using [list_iter] here instead of a bind
   allows us to see the whole pipeline statically, before we've
   actually calculated the rev-deps. *)
let v5 commit =
  let src = fetch commit in
  let bin = build src in
  let ok = test bin in
  Opam.revdeps src
  |> Current.gate ~on:ok
  |> Current.list_iter ~pp:(Git.Commit.pp) (fun s -> s |> fetch |> build |> test)

let test_v5 _switch () =
  Driver.test ~name:"v5" (with_commit v5) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-123" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_v5_nil _switch () =
  let test_commit = Git.Commit.v ~repo:"my/project" ~hash:"456" in
  Driver.test ~name:"v5n" (with_commit v5) @@ function
  | 1 -> Git.complete_clone test_commit
  | 2 -> Docker.complete "image-src-456" ~cmd:["make"; "test"] @@ Ok ()
  | _ -> raise Exit

let test_option ~case commit =
  let src = fetch commit in
  analyse ~lint:case src
  |> Current.option_map (fun linter -> lint src ~linter)
  |> Current.ignore_value

let test_option_some _switch () =
  test_option ~case:(Some "ocamlformat")
  |> with_commit
  |> (fun c -> Driver.test ~name:"option-some" c @@ function
  | 1 -> Git.complete_clone test_commit
  | _ -> raise Exit)

let test_option_none _switch () =
  test_option ~case:None
  |> with_commit
  |> (fun c -> Driver.test ~name:"option-none" c @@ function
    | 1 -> Git.complete_clone test_commit
    | _ -> raise Exit)

(* This is just to check the diagram when the state box is hidden. *)
let test_state _switch () =
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
    Current.Input.const ()
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

module Test_input = struct
  type job_id = string
  type 'a t = ('a Current_term.Output.t * job_id option) Current_incr.t
  let get x = x
end

module Term = Current_term.Make(Test_input)

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

let test_job_id () =
  let pipeline =
    let j = Term.map ignore (job "1") in
    Term.Analysis.job_id j
  in
  let job_id = Current_incr.observe (Term.Executor.run pipeline) in
  Alcotest.(check (result (option string) reject)) "Got job ID" (Ok (Some "1")) job_id

module Cli = Alcotest.Cli.Make(Lwt)

let () =
  Lwt_main.run begin
    Cli.run "test" [
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
      ];
      "terms", [
        Alcotest_lwt.test_case_sync "all_labelled" `Quick test_all_labelled;
        Alcotest_lwt.test_case_sync "job_id"       `Quick test_job_id;
      ];
      "cache", Test_cache.tests;
      "monitor", Test_monitor.tests;
      "job", Test_job.tests;
      "log_matcher", Test_log_matcher.tests;
    ]
  end
