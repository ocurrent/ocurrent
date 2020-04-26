(* This pipeline monitors a GitHub repository and uses Docker to build the
   latest version on the default branch. *)

let program_name = "github"

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let () = Logging.init ()

(* Link for GitHub statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let github_status_of_state = function
  | Ok _              -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m)    -> Github.Api.Status.v ~url `Failure ~description:m

let pipeline ~github ~repo () =
  let head = Github.Api.head_commit github repo in
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08" in
    `Contents (dockerfile ~base)
  in
  Docker.build ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"

let main config mode github repo =
  let engine = Current.Engine.create ~config (pipeline ~github ~repo) in
  let routes =
    Routes.(s "webhooks" / s "github" /? nil @--> Github.webhook) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.required @@
  Arg.pos 0 (Arg.some Github.Repo_id.cmdliner) None @@
  Arg.info
    ~doc:"The GitHub repository (owner/name) to monitor."
    ~docv:"REPO"
    []

let cmd =
  let doc = "Monitor a GitHub repository." in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.Api.cmdliner $ repo),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
