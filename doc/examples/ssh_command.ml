(* Usage: ssh_command.exe host args

   Given a host and arguments, runs the command every 30 minutes.

   e.g. Run `tar -cvzf bin.tar.gz /bin` on host host.example.com
   $ dune exec -- ./doc/examples/ssh_command.exe -- host.example.com tar -cvf bin.tar.gz /bin

   Check the output and run history via http://localhost:8080
*)

let program_name = "current_ssh"

let () = Prometheus_unix.Logging.init ()

let pipeline ~host ~args () =
  let halfhourly = Current_cache.Schedule.v ~valid_for:(Duration.of_min 30) () in
  let ssh = Current_ssh.run ~schedule:halfhourly host ~key:("my-cmd") (Current.return args) in
  Current.all [ ssh ]

let main config mode host args =
  let engine = Current.Engine.create ~config (pipeline ~host ~args) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let host =
  Arg.value @@
  Arg.pos 0 Arg.string "localhost" @@
  Arg.info
    ~doc:"Host to connect to."
    ~docv:"HOST"
    []

let args =
  Arg.value @@
  Arg.pos_right 0 Arg.string ["ls"] @@
  Arg.info
    ~doc:"Command to run."
    ~docv:"ARGS"
    []

let cmd =
  let doc = "Run an SSH command every 30 minutes." in
  let info = Cmd.info program_name ~doc in
  Cmd.v info Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ host $ args))

let () = exit @@ Cmd.eval cmd
