(* Usage: github_app.exe --github-app-id APP_ID \
            --github-private-key-file=secret-key.pem \
            --github-account-allowlist ACCOUNTS \
            --github-webhook-secret-file=github-app-secret-file

   This pipeline is a GitHub app (APP_ID).
   It monitors all GitHub repositories the app is asked to handle that are
   owned by ACCOUNTS, and uses Docker to build the latest version on all
   branches and PRs. Updates to the repository list and git repositories
   are delivered as webhook events from GitHub, a suitable forwarding of
   these events to github_app.ex is required eg smee.io

*)

let program_name = "github_app"

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

(* Limit to one build at a time. *)
let pool = Current.Pool.create ~label:"docker" 1

let () = Prometheus_unix.Logging.init ()

(* Link for GitHub statuses. *)
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
  |> string_of_t

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Map from Current.state to CheckRunStatus *)
let github_check_run_status_of_state ?job_id = function
  | Ok _              -> Github.Api.CheckRunStatus.v ~url ?identifier:job_id (`Completed `Success) ~summary:"Passed"
  | Error (`Active _) -> Github.Api.CheckRunStatus.v ~url ?identifier:job_id `Queued
  | Error (`Msg m)    -> Github.Api.CheckRunStatus.v ~url ?identifier:job_id (`Completed (`Failure m)) ~summary:m

let check_run_status x =
  let+ md = Current.Analysis.metadata x
  and+ state = Current.state x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> github_check_run_status_of_state ?job_id state
  | None -> github_check_run_status_of_state state

let pipeline ~app () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocaml/opam:alpine-3.13-ocaml-4.13" in
    `Contents (dockerfile ~base)
  in
  Github.App.installations app |> Current.list_iter (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  Github.Api.Repo.ci_refs ~staleness:(Duration.of_day 90) repo
  |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  Docker.build ~pool ~pull:false ~dockerfile (`Git src)
  |> check_run_status
  |> Github.Api.CheckRun.set_status head program_name

let main config mode app =
  Lwt_main.run begin
    let has_role = Current_web.Site.allow_all in
    let engine = Current.Engine.create ~config (pipeline ~app) in
    let webhook_secret = Current_github.App.webhook_secret app in
    (* this example does not have support for looking up job_ids for a commit *)
    let get_job_ids = (fun ~owner:_owner ~name:_name ~hash:_hash -> []) in
    let routes =
      Routes.(s "webhooks" / s "github" /? nil @--> Github.webhook ~engine ~get_job_ids ~webhook_secret) ::
      Current_web.routes engine
    in
    let site = Current_web.Site.(v ~has_role) ~name:program_name routes in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Monitor a GitHub app's repositories." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.App.cmdliner))

let () = exit @@ Cmd.eval cmd
