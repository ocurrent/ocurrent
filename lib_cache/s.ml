module type WITH_DIGEST = sig
    type t

    val digest : t -> string
    (** [digest t] is a unique string for [t] that can be used as a primary key in a database.
        Two [t]s are considered equal if they have the same digest. *)
end

module type WITH_MARSHAL = sig
    type t

    val marshal : t -> string
    (** Convert [t] to a form suitable for storage in the database. *)

    val unmarshal : string -> t
    (** Restore a [t] from a string previously produced by [marshal].
        Raise an exception if the value cannot be read (e.g. the format has changed). *)
end

module type OPERATION = sig
  type t
  (** Extra context or configuration information (e.g. credentials to access a remote service). *)

  val id : string
  (** A short and unique name for this operation, used as a filename component and database key.
      e.g. "docker-build" *)

  module Key : WITH_DIGEST
  (** The [id, Key.t] pair uniquely identifies the operation. *)

  val level : t -> Key.t -> Current.Level.t
  (** [level t k] provides an estimate of how risky / expensive this operation is.
      This is useful to perform dry-runs, or limit to local-only effects, etc. *)
end

module type BUILDER = sig
  (** A builder generates an output value from an input key. *)

  include OPERATION

  type job

  module Value : WITH_MARSHAL
  (** The result of a build. *)

  val build : switch:Lwt_switch.t -> t -> job -> Key.t -> (Value.t, [`Msg of string]) result Lwt.t
  (** [build ~switch t j k] builds [k].
      If the switch is turned off, the build should be cancelled.
      Log messages can be written to [j]. *)

  val pp : Key.t Fmt.t
  (** Describe the operation. e.g. ["docker-build $path"]. *)

  val auto_cancel : bool
  (** [true] if an operation should be cancelled if it is no longer needed, or
      [false] to cancel only when the user explicitly requests it. *)
end
