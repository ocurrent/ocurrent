open Current.Syntax    (* let*, let+, etc *)

module Git = Current_git_test
module Docker = Current_docker_test
module Opam = Current_opam_test

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

let fetch = Git.fetch
let build = Docker.build
let test = Docker.run ~cmd:["make"; "test"]
let push = Docker.push

(* A very simple linear pipeline. Given a commit (e.g. the head of
   a PR on GitHub), this returns success if the tests pass on it. *)
let v1 commit =
  commit |> fetch |> build |> test

(* Similar, but here the test step requires both the binary and
   the source (perhaps for the test cases). If the tests pass then
   it deploys the binary too. *)
let v2 commit =
  let src = fetch commit in
  let bin = build src in
  bin |> Current.gate ~on:(test bin) |> push ~tag:"foo/bar"

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

(* Monadic bind is also available if you need to take a decision based
   on the actual source code before deciding on the rest of the pipeline.
   The let** form allows you to name the box.
   The static analysis will only show what happens up to this step until
   it actually runs, after which it will show the whole pipeline. *)
let v4 commit =
  let src = fetch commit in
  "custom-build" |>
  let** src = src in
  if Fpath.to_string src = "src-123" then build (Current.return (Fpath.v "bad")) |> test
  else Current.fail "Wrong hash!"

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
  |> Current.list_iter (fun s -> s |> fetch |> build |> test)

(* Test driver *)

let test_commit =
  Git.Commit.v ~repo:"my/project" ~hash:"123"

module Commit_var = Current.Var(Git.Commit)

let with_analysis ~name ~i (t : unit Current.t) =
  let data =
    let+ a = Current.Analysis.get t in
    Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp a);
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let path = Current.return (Fpath.v (Fmt.strf "%s.%d.dot" name !i)) in
  Current_fs.save path data

let ready i = Lwt.state i#changed <> Lwt.Sleep

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ~name v =
  Git.reset ();
  Docker.reset ();
  (* Perform an initial analysis: *)
  let head = Commit_var.create ~name:"head" (Ok test_commit) in
  let i = ref 1 in
  let trace x watches =
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.unit "()")) x);
    Logs.info (fun f -> f "@[<v>Depends on: %a@]" Fmt.(Dump.list Current.Input.pp_watch) watches);
    begin
      match List.find_opt ready watches with
      | Some i -> Fmt.failwith "Input already ready! %a" Current.Input.pp_watch i
      | None -> ()
    end;
    incr i;
    begin match !i with
    | 2 -> Git.complete_clone test_commit
    | 3 ->
      Docker.complete "image-src-123" ~cmd:["make"; "test"] `Complete;
      Docker.complete "lin-image-src-123" ~cmd:["make"; "test"] `Complete;
      Docker.complete "win-image-src-123" ~cmd:["make"; "test"] `Failed;
      Docker.complete "image-bad" ~cmd:["make"; "test"] `Failed;
    | _ ->
      List.iter (fun w -> w#release) watches;
      raise Exit
    end;
    if not (List.exists ready watches) then raise Exit
  in
  try Lwt_main.run @@ Current.Engine.run ~trace (fun () -> with_analysis ~name ~i @@ v (Commit_var.get head))
  with Exit ->
    Docker.assert_finished ()

let () =
  test ~name:"v1" v1;
  test ~name:"v2" v2;
  test ~name:"v3" v3;
  test ~name:"v4" v4;
  test ~name:"v5" v5
