module Job : sig
  (** Client-side API to contact a job service. *)

  type t = [`Job_8397ef9078537247] Capnp_rpc_lwt.Capability.t
  type id = string

  type status = {
    id : id;
    description : string;
    can_cancel : bool;
    can_rebuild : bool;
  }

  val log : start:int64 -> t -> (string * int64, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [log ~start t] returns bytes from the log starting at offset [start]. *)

  val cancel : t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  val status : t -> (status, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t

  val rebuild : t -> t
  (** [rebuild t] requests a rebuild of [t] and returns the new job. *)

  val approve_early_start : t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** Mark the job as approved to start even if the global confirmation threshold
      would otherwise prevent it. Calling this more than once has no effect. *)
end

module Engine : sig
  (** Client-side API to contact an engine service. *)

  type t = [`Engine_f0961466d2f9bbf5] Capnp_rpc_lwt.Capability.t

  (** {2 Existing Methods} *)

  val active_jobs : t -> (Job.id list, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [active_jobs t] lists the OCurrent jobs that are still being used in the pipeline.
      This includes completed jobs, as long as OCurrent is still ensuring they are up-to-date. *)

  val job : t -> Job.id -> Job.t
  (** [job t id] is the job with the given ID. This does not have to be an active job (but only
      active jobs can be rebuilt). If the job ID is unknown, this operation will resolve to a
      suitable error. *)

  (** {2 Query & History} *)

  type query_params = {
    op : string option;          (** Filter by operation type *)
    ok : bool option;            (** Filter by success/failure *)
    rebuild : bool option;       (** Filter by rebuild-needed flag *)
    job_prefix : string option;  (** Filter by job ID prefix (e.g., date "2024-01-15") *)
  }

  type history_entry = {
    job_id : string;
    build : int64;
    outcome : (string, string) result;  (** Ok value or Error message *)
    ready : float;                       (** Unix timestamp when queued *)
    running : float option;              (** Unix timestamp when started, if ever *)
    finished : float;                    (** Unix timestamp when finished *)
    rebuild : bool;                      (** Whether a rebuild was requested *)
  }

  val query : t -> query_params -> (history_entry list, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [query t params] queries the job history database with the given filters.
      Returns matching entries sorted by finished time (most recent first). *)

  val ops : t -> (string list, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [ops t] lists all known operation types (e.g., "docker-build", "git-clone"). *)

  (** {2 Pipeline Overview} *)

  type stats = {
    ok : int;
    waiting_for_confirmation : int;
    ready : int;
    running : int;
    failed : int;
    blocked : int;
  }

  type pipeline_state =
    | Success
    | Failed of string
    | Active of [ `Ready | `Running | `Waiting_for_confirmation ]

  val pipeline_stats : t -> (stats, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [pipeline_stats t] returns counts of pipeline stages in each state. *)

  val pipeline_state : t -> (pipeline_state, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [pipeline_state t] returns the overall pipeline state. *)

  val pipeline_dot : t -> (string, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [pipeline_dot t] returns the pipeline graph in Graphviz DOT format. *)

  (** {2 Configuration} *)

  type confirm_level = Harmless | Mostly_harmless | Average | Above_average | Dangerous

  val get_confirm_level : t -> (confirm_level option, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [get_confirm_level t] returns the current confirmation threshold, or [None] if
      no confirmation is required. *)

  val set_confirm_level : t -> confirm_level option -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [set_confirm_level t level] sets the confirmation threshold. Pass [None] to disable
      confirmation. *)

  (** {2 Bulk Operations} *)

  type rebuild_result = {
    succeeded : string list;
    failed : string list;
  }

  val rebuild_all : t -> string list -> (rebuild_result, [> `Capnp of Capnp_rpc.Error.t]) result Lwt.t
  (** [rebuild_all t job_ids] attempts to rebuild multiple jobs at once. Returns lists of
      job IDs that succeeded or failed to be queued for rebuild. *)
end

module S : sig
  (** Module signatures for the RPC system. *)

  (** Active pipeline state. *)
  type active = [ `Ready | `Running | `Waiting_for_confirmation ]

  (** Pipeline output type. *)
  type 'a output = ('a, [`Active of active | `Msg of string]) result

  (** Pipeline statistics (re-exported from current_term). *)
  type stats = Current_term.S.stats = {
    ok : int;
    waiting_for_confirmation : int;
    ready : int;
    running : int;
    failed : int;
    blocked : int;
  }

  module type CURRENT = S.CURRENT
  (** The subset of the Current API that the RPC system needs. *)

  module type DB = S.DB
  (** Database operations interface for job history queries. *)

  module Db_stub : DB
  (** A stub DB implementation that returns empty results. *)
end

module Impl (Current : S.CURRENT) : sig
  (** This is used on the server-side to provide access to the OCurrent Engine.
      Create an instance of the functor with [module Rpc = Current_rpc.Impl(Current)].
      We use a functor here just to avoid having Current_rpc depend on Current,
      which would be annoying for RPC clients.

      This functor uses [Current_cache.Db] for job history queries, so the
      [query] and [ops] RPC methods will return data from the job database. *)

  val job : engine:Current.Engine.t -> Job.id -> Job.t
  (** [job ~engine id] is a Cap'n Proto job service backed by [engine]. *)

  val engine : Current.Engine.t -> Engine.t
  (** [engine e] is a Cap'n Proto engine service backed by [e]. *)
end

module Client : module type of Client
(** Unified RPC client with cmdliner integration.
    See {!Client} for details on embedding in applications. *)

module Impl_with_db (Current : S.CURRENT) (Db : S.DB) : sig
  (** Functor that takes an explicit [Db] module for job history queries.
      This is primarily useful for testing with mock databases.
      For normal use, prefer {!Impl} which uses [Current_cache.Db]. *)

  val job : engine:Current.Engine.t -> Job.id -> Job.t
  (** [job ~engine id] is a Cap'n Proto job service backed by [engine]. *)

  val engine : Current.Engine.t -> Engine.t
  (** [engine e] is a Cap'n Proto engine service backed by [e]. *)
end
