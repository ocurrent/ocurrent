open! Current.Syntax    (* (not actually needed for this example) *)

module Git = Current_git
module Docker = Current_docker

let dotfile = Fpath.v "pipeline.dot"

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let () = Logging.init ()

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull src in
  Docker.run image ~args:["dune"; "exec"; "--"; "examples/docker_build_local.exe"; "--help"]

let main config repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (Logging.with_dot ~dotfile (pipeline ~repo)) in
  Lwt_main.run begin
    Current.Engine.thread engine
  end

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
