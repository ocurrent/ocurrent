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

(** A builder generates an output value from an input key. *)
module type BUILDER = sig
  include OPERATION

  module Value : WITH_MARSHAL
  (** The result of a build. *)

  val build :
    t -> Current.Job.t -> Key.t ->
    Value.t Current.or_error Lwt.t
  (** [build t j k] builds [k].
      Call [Job.start j] once any required resources have been acquired.
      Log messages can be written to [j]. *)

  val pp : Key.t Fmt.t
  (** Describe the operation. e.g. ["docker build $src"]. *)

  val auto_cancel : bool
  (** [true] if an operation should be cancelled if it is no longer needed, or
      [false] to cancel only when the user explicitly requests it. *)
end

(** A publisher sets a key to a value. *)
module type PUBLISHER = sig
  include OPERATION

  module Value : WITH_DIGEST
  (** The value to publish. *)

  module Outcome : WITH_MARSHAL
  (** Extra information about the result, if any.
      Usually this is just [Current.Unit]. *)

  val publish :
    t -> Current.Job.t -> Key.t -> Value.t ->
    Outcome.t Current.or_error Lwt.t
  (** [publish t j k v] sets output [k] to value [v].
      Call [Job.start j] once any required resources have been acquired.
      Log messages can be written to [j]. *)

  val pp : (Key.t * Value.t) Fmt.t
  (** Describe the operation. e.g. ["docker push $tag"]. *)

  val auto_cancel : bool
  (** This affects what happens if we're in the process of outputting a value and
      then we decide to output a different value instead.
      If [true], the old operation will be cancelled immediately.
      If [false], the old operation will run to completion first. *)
end

(** The most general API.
    {!BUILDER} and {!PUBLISHER} are just special cases of this. *)
module type GENERIC = sig

  include OPERATION

  module Value : WITH_DIGEST
  (** A second input value, which is not part of the key
      (so there can be only one value for any key at one time).
      For outputs, this is used to hold the value to which the output should be set.
      For builds, non-essential inputs can be placed here instead of in the key
      so that the old outcome can be latched for minor changes. For example, when
      testing a Git commit the commit to test would be the key, but the latest
      version of the test platform might be the value. *)

  module Outcome : WITH_MARSHAL
  (** The output of the pipeline stage. *)

  val run :
    t -> Current.Job.t -> Key.t -> Value.t ->
    Outcome.t Current.or_error Lwt.t
  (** [run t j k v] performs the operation.
      Call [Job.start j] once any required resources have been acquired.
      Log messages can be written to [j]. *)

  val pp : (Key.t * Value.t) Fmt.t
  (** Describe the operation. e.g. ["docker push $tag"]. *)

  val auto_cancel : bool
  (** This affects what happens if we're in the process of outputting a value and
      then we decide to output a different value instead.
      If [true], the old operation will be cancelled immediately.
      If [false], the old operation will run to completion first. *)

  val latched : bool
  (** This controls what happens to the reported outcome while changing to a new value.
      If [true], we continue to report the previous outcome until the new job is complete.
      If [false], the output is set to active during the update. *)
end
