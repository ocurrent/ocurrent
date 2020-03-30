(** Cache build results in memory and on disk.
    A cache maps keys to values. Looking up a key that isn't known starts a new
    build to create it. *)

module S = S

module Schedule : sig
  type t

  val v : ?valid_for:Duration.t -> unit -> t
  (** Create a new configuration.
      @param valid_for Consider a cached entry invalid after this long
   *)
end

module Make (B : S.BUILDER) : sig
  val get : ?schedule:Schedule.t -> B.t -> B.Key.t -> B.Value.t Current.Primitive.t
  (** [get b k] is a term for the result of building [k]. *)

  val invalidate : B.Key.t -> unit
  (** [invalidate key] removes key from the cache. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Output (P : S.PUBLISHER) : sig
  val set : ?schedule:Schedule.t -> P.t -> P.Key.t -> P.Value.t -> P.Outcome.t Current.Primitive.t
  (** [set p k v] is a term for the result of setting [k] to [v]. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Db : sig
  type entry = {
    job_id : string;
    build : int64;            (* Build number (increases for rebuilds). *)
    value : string;
    outcome : string Current.or_error;
    ready : float;            (* When the job was ready to begin. *)
    running : float option;   (* When it actually started running (e.g. after confirmation). *)
    finished : float;         (* When it finished (successfully or not). *)
    rebuild : bool;           (* If [true], then a rebuild was requested. *)
  }

  val init : unit -> unit
  (** Ensure that the database tables have been created.
      This is useful if you need to refer to them in your own SQL. *)

  val query : ?op:string -> ?ok:bool -> ?rebuild:bool -> unit -> entry list
  (** Search the database for matching records.
      @param op : if present, restrict to results from the named builder or publisher
      @param ok : if present, restrict results to passing (ok=true) or failing (ok=false) results.
      @param rebuild : if present, restrict results to ones where the rebuild flag matches this. *)
end
