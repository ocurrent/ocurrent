(** Cache build results in memory (or, TODO, on disk).
    A cache maps keys to values. Looking up a key that isn't known starts a new
    build to create it. *)

module type BUILDER = sig
  type t
  (** Extra build context or configuration information (e.g. credentials to
      access a remote service). *)

  module Key : sig
    (** A key is an input that needs to be built. *)

    include Set.OrderedType
  end

  module Value : sig
    (** The result of a successful build. *)

    type t
  end

  val pp : Key.t Fmt.t
  (** Describe the operation that builds a [Value.t] from the given [key]. *)

  val build : switch:Lwt_switch.t -> t -> Key.t -> (Value.t, [`Msg of string]) result Lwt.t
  (** [build ~switch t k] builds [k]. If the switch is turned off, the build should be cancelled. *)

  val auto_cancel : bool
  (** [true] if a build should be cancelled if it is no longer needed, or
      [false] to cancel only when the user explicitly requests it. *)
end

module Make (B : BUILDER) : sig
  val get : B.t -> B.Key.t -> B.Value.t Current.t
  (** [get b k] is a term for the result of building [k]. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end
