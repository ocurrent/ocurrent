(** Cache build results in memory (or, TODO, on disk).
    A cache maps keys to values. Looking up a key that isn't known starts a new
    build to create it. *)

module Job : sig
  type t

  val write : t -> string -> unit
  (** [write t data] appends [data] to the log. *)

  val log : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log t fmt] appends a formatted message to the log, with a newline added at the end. *)

  val fd : t -> Unix.file_descr
end

module type BUILDER = sig
  type t
  (** Extra build context or configuration information (e.g. credentials to
      access a remote service). *)

  val id : string
  (** A short and unique name for this builder, used as a filename component and database key.
      e.g. "docker-build" *)

  module Key : sig
    (** A key is an input that needs to be built. *)

    type t

    val digest : t -> string
    (** [digest t] is a unique string for [t] that can be used as a primary key in a database.
        Two [t]s are considered equal if they have the same digest. *)
  end

  module Value : sig
    (** The result of a successful build. *)

    type t

    val marshal : t -> string
    (** Convert [t] to a form suitable for storage in the database. *)

    val unmarshal : string -> t
    (** Restore a [t] from a string previously produced by [marshal].
        Raise an exception if the value cannot be read (e.g. the format has changed). *)
  end

  val pp : Key.t Fmt.t
  (** Describe the operation that builds a [Value.t] from the given [key]. *)

  val build : switch:Lwt_switch.t -> t -> Job.t -> Key.t -> (Value.t, [`Msg of string]) result Lwt.t
  (** [build ~switch t j k] builds [k].
      If the switch is turned off, the build should be cancelled.
      Log messages can be written to [j]. *)

  val auto_cancel : bool
  (** [true] if a build should be cancelled if it is no longer needed, or
      [false] to cancel only when the user explicitly requests it. *)

  val level : t -> Key.t -> Current.Level.t
  (** [level t k] provides an estimate of how risky / expensive this operation is.
      This is useful to perform dry-runs, or limit to local-only effects, etc. *)
end

module Schedule : sig
  type t

  val v : ?valid_for:Duration.t -> unit -> t
  (** Create a new configuration.
      @param valid_for Consider a cached entry invalid after this long
   *)
end

module Make (B : BUILDER) : sig
  val get : ?schedule:Schedule.t -> B.t -> B.Key.t -> B.Value.t Current.t
  (** [get b k] is a term for the result of building [k]. *)

  val invalidate : B.Key.t -> unit
  (** [invalidate key] removes key from the cache. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Process : sig
  val exec :
    ?switch:Lwt_switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
    unit Current.or_error Lwt.t
  (** [exec ~job cmd] uses [Lwt_process] to run [cmd], with output to [job]'s log.
      @param switch If this is turned off, the process is terminated.
      @param stdin Data to write to stdin before closing it. *)

  val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_tmpdir fn] creates a temporary directory, runs [fn tmpdir], and then deletes the directory
      (recursively).
      @param prefix Allows giving the directory a more meaningful name (for debugging). *)
end

(**/**)

(* For unit tests we need our own test clock: *)

val timestamp : (unit -> float) ref
val sleep : (float -> unit Lwt.t) ref
