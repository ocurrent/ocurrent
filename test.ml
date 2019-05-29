open OCurrent.Syntax    (* let*, let+, etc *)
open Primitives         (* fetch, build, test, etc *)

(* A very simple linear pipeline. Given a commit (e.g. the head of
   a PR on GitHub, this returns success if the tests pass on it. *)
let v1 commit =
  commit |> fetch |> build |> test

(* Similar, but here the test step requires both the binary and
   the source (perhaps for the test cases). If the tests pass then
   it deploys the binary too. *)
let v2 commit =
  let src = fetch commit in
  let bin = build src in
  bin |> OCurrent.gate ~on:(test ~src bin) |> deploy

(* Build Linux, Mac and Windows binaries. If *all* tests pass (for
   all platforms) then deploy all binaries. *)
let v3 commit =
  let platforms = ["lin"; "mac"; "win"] in
  let src = fetch commit in
  let binaries = List.map (fun p -> build ~on:p src) platforms in
  let tests = OCurrent.all @@ List.map (test ~src) binaries in
  let gated_deploy x = x |> OCurrent.gate ~on:tests |> deploy in
  OCurrent.all @@ List.map gated_deploy binaries

(* Monadic bind is also available if you need to take a decision based
   on the actual source code before deciding on the rest of the pipeline.
   The let** form allows you to name the box.
   The static analysis will only show what happens up to this step until
   it actually runs, after which it will show the whole pipeline. *)
let v4 commit =
  let src = fetch commit in
  "custom-build" |>
  let** src = src in
  if (src :> string) = "src-123" then build (OCurrent.return src) |> test
  else OCurrent.fail "Wrong hash!"

(* The opam-repo-ci pipeline. Build the package and test it.
   If the tests pass then query for the rev-deps and build and
   test each of them. Using [list_iter] here instead of a bind
   allows us to see the whole pipeline statically, before we've
   actually calculated the rev-deps. *)
let v5 commit =
  let src = fetch commit in
  let bin = build src in
  let ok = test ~src bin in
  revdeps src
  |> OCurrent.gate ~on:ok
  |> OCurrent.list_iter (fun s -> s |> fetch |> build |> test)

(* Test driver *)

let output_dir = Fpath.v "./results"

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let ( / ) = Fpath.( / )

let write_dot_to_channel ch x =
  let f = Format.formatter_of_out_channel ch in
  OCurrent.pp_dot f x;
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

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ~name v =
  let s = v OCurrent.pending in   (* The initial static analysis *)
  Fmt.pr "Run: %a@." OCurrent.pp s;
  write_dot ~name:(name ^ "-before") s;
  let x = v (OCurrent.return test_commit) in  (* After supplying the input *)
  write_dot ~name:(name ^ "-after") x;
  Fmt.pr "--> %a@." (Dyn.pp (Fmt.unit "()")) (OCurrent.run x)

let () =
  test ~name:"v1" v1;
  test ~name:"v2" v2;
  test ~name:"v3" v3;
  test ~name:"v4" v4;
  test ~name:"v5" v5
