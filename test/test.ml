open Current.Syntax    (* let*, let+, etc *)

module StringMap = Map.Make(String)

let fetch = Current_git.fetch
let build = Current_docker.build
let test = Current_docker.run ~cmd:["make"; "test"]
let push = Current_docker.push

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
  Current_opam.revdeps src
  |> Current.gate ~on:ok
  |> Current.list_iter (fun s -> s |> fetch |> build |> test)

(* Test driver *)

let output_dir = Fpath.v "./results"

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let ( / ) = Fpath.( / )

let write_dot_to_channel ch x =
  let f = Format.formatter_of_out_channel ch in
  Current.Static.pp_dot f x;
  Format.pp_print_flush f ();
  Ok ()

let write_dot ~name s =
  let open Bos in
  let _ : bool = Bos.OS.Dir.create output_dir |> or_die in
  let dotfile = output_dir / Fmt.strf "%s.dot" name in
  let svgfile = output_dir / Fmt.strf "%s.svg" name in
  Bos.OS.File.with_oc dotfile write_dot_to_channel s |> or_die |> or_die;
  Fmt.pr "Wrote %a@." Fpath.pp dotfile;
  let dot = Cmd.(v "dot" % "-Tsvg" % p dotfile % "-o" % p svgfile) in
  match OS.Cmd.run dot with
  | Ok () -> ()
  | Error (`Msg x) -> failwith x

let test_commit =
  Current_git.commit ~repo:"my/project" ~hash:"123"

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ~name v =
  Current_git.reset ();
  (* Perform an initial analysis: *)
  let input = Current.track "PR head" @@ Current.return test_commit in
  let s, x, inputs = Current.run @@ v input in
  Fmt.pr "@.Before: %a@." Current.Static.pp s;
  Fmt.pr "--> %a@." (Current.pp_output (Fmt.unit "()")) x;
  Fmt.pr "Depends on: %a@." Fmt.(Dump.list string) inputs;
  write_dot ~name:(name ^ "-before") s;
  Current_git.complete_clone test_commit;
  (* After supplying the input: *)
  let s, x, inputs = Current.run @@ v input in
  Fmt.pr "@.After: %a@." Current.Static.pp s;
  write_dot ~name:(name ^ "-after") s;
  Fmt.pr "--> %a@." (Current.pp_output (Fmt.unit "()")) x;
  Fmt.pr "Depends on: %a@." Fmt.(Dump.list string) inputs

let () =
  test ~name:"v1" v1;
  test ~name:"v2" v2;
  test ~name:"v3" v3;
  test ~name:"v4" v4;
  test ~name:"v5" v5
