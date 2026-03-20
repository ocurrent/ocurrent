(** The subset of the Current API that the RPC system needs.
    This is duplicated here to avoid making RPC clients depend on
    the "current" service implementation package. *)

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

module type CURRENT = sig
  (** The term type representing pipeline computations. *)
  type 'a term

  class type actions = object
    method pp : Format.formatter -> unit
    method rebuild : (unit -> string) option
  end

  module Job : sig
    type t
    module Map : Map.S with type key = string
    val log_path : string -> (Fpath.t, [`Msg of string]) result
    val lookup_running : string -> t option
    val wait_for_log_data : t -> unit Lwt.t
    val approve_early_start : t -> unit
    val cancel : t -> string -> unit
    val cancelled_state : t -> (unit, [`Msg of string]) result
  end

  (** Confirmation levels for operations. *)
  module Level : sig
    type t

    val values : t list
    (** All possible levels, in order. *)

    val to_string : t -> string
    val of_string : string -> (t, [> `Msg of string]) result
  end

  (** Engine configuration. *)
  module Config : sig
    type t

    val get_confirm : t -> Level.t option
    (** Get the current confirmation threshold, if any. *)

    val set_confirm : t -> Level.t option -> unit
    (** Set the confirmation threshold. [None] means no confirmation required. *)
  end

  (** Pipeline metadata for jobs. *)
  module Metadata : sig
    type t = {
      job_id : string option;
      update : active option;
    }
  end

  (** Pipeline analysis module. *)
  module Analysis : sig
    val stat : 'a term -> stats
    (** [stat t] returns statistics about the pipeline states. *)

    val pp_dot :
      env:(string * string) list ->
      collapse_link:(k:string -> v:string -> string option) ->
      job_info:(Metadata.t -> active option * string option) ->
      Format.formatter -> 'a term -> unit
    (** [pp_dot ~env ~collapse_link ~job_info ppf t] outputs the pipeline as a DOT graph. *)
  end

  module Engine : sig
    type t

    type results = {
      value : unit output;
      jobs : actions Job.Map.t;
    }

    val state : t -> results
    val config : t -> Config.t
    (** Get the engine configuration. *)

    val pipeline : t -> unit term
    (** [pipeline t] returns the current pipeline term. *)
  end
end

(** Database operations interface. This matches Current_cache.Db exactly
    so applications can use it directly. *)
module type DB = sig
  type entry = {
    job_id : string;
    build : int64;
    value : string;
    outcome : (string, [`Msg of string]) result;
    ready : float;
    running : float option;
    finished : float;
    rebuild : bool;
  }

  val query :
    ?op:string ->
    ?ok:bool ->
    ?rebuild:bool ->
    ?job_prefix:string ->
    unit -> entry list
  (** Query job history with optional filters. *)

  val ops : unit -> string list
  (** List all known operation types. *)
end

(** A stub DB implementation that returns empty results.
    @deprecated No longer used since Impl uses Current_cache.Db directly. *)
module Db_stub : DB = struct
  type entry = {
    job_id : string;
    build : int64;
    value : string;
    outcome : (string, [`Msg of string]) result;
    ready : float;
    running : float option;
    finished : float;
    rebuild : bool;
  }

  let query ?op:_ ?ok:_ ?rebuild:_ ?job_prefix:_ () = []
  let ops () = []
end
