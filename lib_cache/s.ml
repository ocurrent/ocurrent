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
end

module type BUILDER = sig
  (** A builder generates an output value from an input key. *)

  include OPERATION

  module Value : WITH_MARSHAL
  (** The result of a build. *)

  val build :
    switch:Current.Switch.t ->
    t -> Current.Job.t -> Key.t ->
    Value.t Current.or_error Lwt.t
  (** [build ~switch t j k] builds [k].
      Call [Job.start j] once any required resources have been acquired.
      If the switch is turned off, the build should be cancelled.
      Log messages can be written to [j]. *)

  val pp : Key.t Fmt.t
  (** Describe the operation. e.g. ["docker build $src"]. *)

  val auto_cancel : bool
  (** [true] if an operation should be cancelled if it is no longer needed, or
      [false] to cancel only when the user explicitly requests it. *)
end

module type PUBLISHER = sig
  (** A publisher sets a key to a value. *)

  include OPERATION

  module Value : WITH_DIGEST
  (** The value to publish. *)

  module Outcome : WITH_MARSHAL
  (** Extra information about the result, if any.
      Usually this is just [Current.Unit]. *)

  val publish :
    switch:Current.Switch.t -> t -> Current.Job.t -> Key.t -> Value.t ->
    Outcome.t Current.or_error Lwt.t
  (** [publish ~switch t j k v] sets output [k] to value [v].
      Call [Job.start j] once any required resources have been acquired.
      If the switch is turned off, the operation should be cancelled.
      Log messages can be written to [j]. *)

  val pp : (Key.t * Value.t) Fmt.t
  (** Describe the operation. e.g. ["docker push $tag"]. *)

  val auto_cancel : bool
  (** This affects what happens if we're in the process of outputting a value and
      then we decide to output a different value instead.
      If [true], the old operation will be cancelled immediately.
      If [false], the old operation will run to completion first. *)
end
