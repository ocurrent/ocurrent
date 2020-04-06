(* This is similar to docker_build_local.ml, but exposes a Cap'n Proto endpoint
   so that it can be controlled remotely.

   Run this as e.g.

   dune exec -- ./examples/rpc_server.exe \
      --capnp-secret-key-file=secret-key.pem \
      --capnp-listen-address=unix:/tmp/ocurrent.sock

   This will write out a "./engine.cap" file, which clients
   can use to connect. See rpc_client.ml for an example. *)

open Lwt.Infix

module Git = Current_git
module Docker = Current_docker.Default
module Rpc = Current_rpc.Impl(Current)

let () = Logging.init ()

(* Where we write the connection details containing the connection address and
   authorisation token. *)
let cap_file = "engine.cap"

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let timeout = Duration.of_min 50    (* Max build time *)

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull ~timeout (`Git src) in
  Docker.run image ~args:["dune"; "exec"; "--"; "examples/docker_build_local.exe"; "--help"]

let main config mode capnp repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id capnp "engine" in
  let restore = Capnp_rpc_net.Restorer.single service_id (Rpc.engine engine) in
  Logging.run begin
    Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
    let uri = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
    let ch = open_out cap_file in
    output_string ch (Uri.to_string uri ^ "\n");
    close_out ch;
    Logs.app (fun f -> f "Wrote capability reference to %S" cap_file);
    let routes = Current_web.routes engine in
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode routes;
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

let man = [
  `S Manpage.s_options;
  (* Update once https://github.com/mirage/capnp-rpc/pull/163 is merged: *)
  (* `S "CAP'N PROTO OPTIONS"; *)
]

let cmd =
  let doc = "A build server that can be controlled via Cap'n Proto RPC" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Capnp_rpc_unix.Vat_config.cmd $ repo),
  Term.info "rpc_server" ~doc ~man

let () = Term.(exit @@ eval cmd)
