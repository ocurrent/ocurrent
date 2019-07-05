open! Current.Syntax    (* (not actually needed for this example) *)

module Git = Current_git
module Docker = Current_docker.Default

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let () = Logging.init ()

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull src in
  Docker.run image ~args:["dune"; "exec"; "--"; "examples/docker_build_local.exe"; "--help"]

let main config mode repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode engine;
    ]
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
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo),
  Term.info "docker_build_local" ~doc

let () = Term.(exit @@ eval cmd)
