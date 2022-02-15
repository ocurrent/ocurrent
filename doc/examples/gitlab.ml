(* Usage: gitlab.exe GITLAB_USER/REPO/PROJECT_ID --gitlab-token-file GITLAB-TOKEN-FILE \
            --gitlab-webhook-secret-file GITLAB-WEBHOOK-SECRET

   This pipeline monitors a GitLab repository and uses Docker to build the
   the latest version on all branches and Merge Requests. Updates to the GitLab
   repository are delivered as webhooks to `/webhooks/gitlab`, some suitable configuration
   and forwarding of these events is required. eg smee.io
*)

let program_name = "gitlab"

open Current.Syntax

module Git = Current_git
module Gitlab = Current_gitlab
module Docker = Current_docker.Default

(* Limit to one build at a time. *)
let pool = Current.Pool.create ~label:"docker" 1

let () = Prometheus_unix.Logging.init ()

(* Link for GitLab statuses. *)
let url = Uri.of_string "http://localhost:8080"

(* Generate a Dockerfile for building all the opam packages in the build context. *)
let dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  run "sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam" @@
  run "opam init --reinit -n" @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  run "opam install . --show-actions --deps-only -t" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let gitlab_status_of_state = function
  | Ok _              -> Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:program_name
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:program_name
  | Error (`Msg m)    -> Gitlab.Api.Status.v ~url `Failure ~description:m ~name:program_name

let pipeline ~gitlab ~repo_id () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocaml/opam:alpine-3.13-ocaml-4.13" in
    `Contents (dockerfile ~base)
  in
  Gitlab.Api.ci_refs gitlab ~staleness:(Duration.of_day 90) repo_id
  |> Current.list_iter (module Gitlab.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Gitlab.Api.Commit.id head) in

  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map gitlab_status_of_state
  |> Gitlab.Api.Commit.set_status head program_name

let main config mode gitlab repo =
  let has_role = Current_web.Site.allow_all in
  let engine = Current.Engine.create ~config (pipeline ~gitlab ~repo_id:repo) in
  let routes =
    Routes.(s "webhooks" / s "gitlab" /? nil @--> Gitlab.webhook ~webhook_secret:(Gitlab.Api.webhook_secret gitlab)) ::
    Current_web.routes engine
  in
  let site = Current_web.Site.(v ~has_role) ~name:program_name routes in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Monitor a GitLab repository." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_gitlab.Api.cmdliner $ Current_gitlab.Repo_id.cmdliner))

let () = exit @@ Cmd.eval cmd
