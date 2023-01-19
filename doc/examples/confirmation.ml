(** Show how to use the [`Waiting-for-activation] state to
    execute a task only when confirm. Roles should be set to
    have a better control on who can execute these actions.

    To run the program, you have to run this command in the terminal...
    $ dune exec -- ./doc/examples/confirmation.exe

    and check the state at [http://localhost:8080]. You can click on the show
    node and click the button to run the step. *)

open Lwt.Infix
open Current.Syntax

module Git = Current_git

let program_name = "confirmation"

(* This is a minimal cache example to show the confirmation lock with
   OCurrent. *)
module Show = struct
    type t = No_context
    let id = "show-cmd"
    module Key = Current.String
    module Value = Current.Unit

    let build No_context job key =
        (* We specify this job as [Dangerous] so the confirmation will be hold. *)
        Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
            Current.Job.log job "You are using this commit %s" key;
            Lwt.return @@ Ok ()

    let pp = Key.pp
    let auto_cancel = true
end

(* We create the cache using the OCurrent function. *)
module Show_cache = Current_cache.Make(Show)

let show hash =
    Current.component "show" |>
    let> key = hash in
    Show_cache.get Show.No_context key

let () = Prometheus_unix.Logging.init ()

(* The pipeline will execute these actions:
    1. Fetch a commit
    2. Wait for confirmation
    3. Display the commit once the stage is confirmed. *)
let pipeline ~repo () =
    let commit = Git.Local.head_commit repo in
    let hash = Current.map Git.Commit.hash commit in
    show hash

let main mode repo =
    (* Here, we set the config to request a confirmation above job with [Average] value. *)
    let config = Current.Config.v ~confirm:Current.Level.Average () in
    Lwt_main.run begin
        let repo = Git.Local.v (Fpath.v repo) in
        let engine = Current.Engine.create ~config (pipeline ~repo) in
        let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
        Lwt.choose [
            Current.Engine.thread engine;
            Current_web.run ~mode site
        ]
    end

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
  Cmd.v info Term.(term_result (const main $ Current_web.cmdliner $ repo))

let () = exit @@ Cmd.eval cmd
