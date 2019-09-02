(* This pipeline is GitHub app.
   It monitors all GitHub repositories the app is asked to handle, and uses
   Docker to build the latest version on the default branch. *)

open Current.Syntax

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default

let () = Logging.init ()

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
  | Ok _ -> `Success
  | Error (`Active _) -> `Pending
  | Error (`Msg _) -> `Failure

let pipeline ~app () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "ocurrent/opam:alpine-3.10-ocaml-4.08" in
    dockerfile ~base
  in
  Github.App.installations app |> Current.list_iter ~pp:Github.Installation.pp @@ fun installation ->
  let github = Current.map Github.Installation.api installation in
  let repos = Github.Installation.repositories installation in
  repos |> Current.list_iter ~pp:Github.Repo_id.pp @@ fun repo ->
  let head = Github.Api.head_commit_dyn github repo in
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  Docker.build ~pull:false ~dockerfile (`Git src)
  |> Current.state
  |> Current.map github_status_of_state
  |> Github.Api.Commit.set_status head "ocurrent"

let webhooks = [
  "github", Github.input_webhook
]

let main config mode app =
  Logging.run begin
    let engine = Current.Engine.create ~config (pipeline ~app) in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode ~webhooks engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Monitor a GitHub app's repositories." in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.App.cmdliner),
  Term.info "github" ~doc

let () = Term.(exit @@ eval cmd)
