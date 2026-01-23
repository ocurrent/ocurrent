(** Unified RPC client for OCurrent pipelines.

    This module provides client operations and cmdliner integration for
    interacting with OCurrent pipelines via Cap'n Proto RPC.

    {2 Usage in Applications}

    To embed client commands in your application:

    {[
      let () =
        let main_cmd = (* your main command *) in
        let client_cmd = Current_rpc.Client.Cmdliner.client_cmd
          ~name:"client"
          ~cap_file:"/path/to/default.cap"
          ()
        in
        let cmd = Cmd.group info [main_cmd; client_cmd] in
        exit @@ Cmd.eval cmd
    ]}

    Then users can run:
    {v
      myapp client overview
      myapp client jobs
      myapp client log <job-id>
    v}

    {2 Standalone Client}

    For a standalone client binary:

    {[
      let () =
        let cmd = Current_rpc.Client.Cmdliner.standalone_cmd ~name:"rpc-client" () in
        exit @@ Cmd.eval cmd
    ]}
*)

(** {2 Client Operations}

    These functions perform individual RPC operations. They can be used
    directly if you need programmatic access rather than CLI. *)

module Ops : sig
  val overview : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** Show pipeline statistics and state. *)

  val jobs : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** List active jobs. *)

  val status : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [status engine job_id] shows the status of a specific job. *)

  val log : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [log engine job_id] streams the log of a job. *)

  val cancel : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [cancel engine job_id] cancels a running job. *)

  val rebuild : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [rebuild engine job_id] rebuilds a job and streams its log. *)

  val start : Engine.t -> string -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [start engine job_id] approves early start for a waiting job. *)

  val query :
    Engine.t ->
    op:string option ->
    ok:bool option ->
    rebuild:bool option ->
    job_prefix:string option ->
    (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** Query job history with optional filters. *)

  val ops : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** List operation types. *)

  val dot : Engine.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** Output pipeline as DOT graph. *)

  val confirm : Engine.t -> string option -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [confirm engine None] gets the current level; [confirm engine (Some level)] sets it. *)

  val rebuild_all : Engine.t -> string list -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** Rebuild multiple jobs. *)
end

(** {2 Connection Helpers} *)

val connect : Uri.t -> Engine.t Lwt.t
(** [connect cap_uri] connects to an engine using a capability URI. *)

val with_engine : Uri.t -> (Engine.t -> 'a Lwt.t) -> 'a Lwt.t
(** [with_engine cap_uri f] connects to an engine, runs [f], then disconnects. *)

(** {2 Cmdliner Integration} *)

module Cmdliner : sig
  val cap_uri : Uri.t Cmdliner.Term.t
  (** Term for the --cap/-c option to specify the capability file/URI. *)

  val client_cmd : ?name:string -> ?cap_file:string -> unit -> unit Cmdliner.Cmd.t
  (** [client_cmd ?name ?cap_file ()] creates a command group with all client subcommands.

      @param name The command name (default: "client")
      @param cap_file Default capability file path. If provided, --cap becomes optional.

      Use this to embed client commands in your application:
      {[
        let cmd = Cmd.group info [main_cmd; Current_rpc.Client.Cmdliner.client_cmd ()]
      ]} *)

  val standalone_cmd : ?name:string -> unit -> unit Cmdliner.Cmd.t
  (** [standalone_cmd ?name ()] creates a standalone client command.

      @param name The program name (default: "ocurrent-rpc")

      Use this to create a standalone client binary:
      {[
        let () = exit @@ Cmd.eval (Current_rpc.Client.Cmdliner.standalone_cmd ())
      ]} *)

  (** {3 Individual Subcommands}

      These are exposed in case you want to cherry-pick specific commands. *)

  val overview_cmd : unit Cmdliner.Cmd.t
  val jobs_cmd : unit Cmdliner.Cmd.t
  val status_cmd : unit Cmdliner.Cmd.t
  val log_cmd : unit Cmdliner.Cmd.t
  val cancel_cmd : unit Cmdliner.Cmd.t
  val rebuild_cmd : unit Cmdliner.Cmd.t
  val start_cmd : unit Cmdliner.Cmd.t
  val query_cmd : unit Cmdliner.Cmd.t
  val ops_cmd : unit Cmdliner.Cmd.t
  val dot_cmd : unit Cmdliner.Cmd.t
  val confirm_cmd : unit Cmdliner.Cmd.t
  val rebuild_all_cmd : unit Cmdliner.Cmd.t

  val subcommands : unit Cmdliner.Cmd.t list
  (** All client subcommands as a list. *)
end
