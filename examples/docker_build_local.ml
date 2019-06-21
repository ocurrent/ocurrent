open Current.Syntax

module Git = Current_git
module Docker = Current_docker

let dotfile = Fpath.v "pipeline.dot"

(* Configure logging *)

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build src in
  Docker.run image ~args:["dune"; "exec"; "--"; "examples/docker_build_local.exe"; "--help"]

(* Render pipeline as dot file *)
let pipeline ~repo () =
  let result = pipeline ~repo () in
  let dot_data =
    let+ a = Current.Analysis.get result in
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let* () = Current_fs.save (Current.return dotfile) dot_data in
  result

let main config repo =
  let repo = Git.Local.v (Fpath.v repo) in
  Lwt_main.run (Current.Engine.run ~config (pipeline ~repo))

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value @@
  Arg.pos 0 Arg.dir (Sys.getcwd ()) @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  Term.(const main $ Current.Config.cmdliner $ repo),
  Term.info "docker_build_local" ~doc

let () = Term.(exit @@ eval cmd)
