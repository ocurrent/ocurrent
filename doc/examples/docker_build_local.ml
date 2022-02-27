let program_name = "docker_build_local"

module Git = Current_git
module Docker = Current_docker.Default

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let timeout = Duration.of_min 50    (* Max build time *)

let () = Prometheus_unix.Logging.init ()

(* included in doc/example_pipelines.md as code snippet *)
[@@@part "pipeline"]
(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull ~timeout (`Git src) in
  Docker.run image ~args:["dune"; "exec"; "--"; "docker_build_local"; "--help"]

[@@@part "end-pipeline"]

let main config mode repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
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
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
