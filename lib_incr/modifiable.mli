(** The low-level modifiable interface.
    [Current_incr] provides a similar but safer wrapper around this. *)

type 'a t
(** A modifiable value. *)

type changeable
(** Represents the result of a changeable computation. Internally, [type changeable = unit].
    This is to force changeable computations to end with a write operation. *)

val create :
  ('a t -> changeable) ->
  'a t
(** [create init] creates a new modifiable [t] and then uses [init t] to initialise it.
    [init] must use [x] exactly once, at the end, in a write operation. *)

val read : 'a t -> ('a -> changeable) -> changeable
(** [read t fn] calls [fn] with the current value of [t], and also records this access.
    If [t] later changes, [fn] will be run again as needed to update its result. *)

val write : eq:('a -> 'a -> bool) -> 'a t -> 'a -> changeable
(** [write ~eq t v] sets [t] to [v] if [eq] says the new value is different to the existing
    one (if any). This should only be used as the last step of the [init]
    function passed to [create], to write to the given modifiable. *)

val change : eq:('a -> 'a -> bool) -> 'a t -> 'a -> unit
(** [change ~eq t v] is similar to [write], but is for use by the top-level for setting inputs
    to the computation. *)

val deref : 'a t -> 'a
(** [deref t] is the current value of [t] (which may be stale if [change] has
    been used since the last [propagate] *)

val propagate : unit -> unit
(** [propagate ()] updates the computation by rerunning any computations that
    might have out-of-date results. *)

val on_release : (unit -> unit) -> unit
(** [on_release fn] registers [fn ()] to be called if the containing
    computation needs to be re-evaluated. *)

module Separate (Map : Map.S) : sig
  (** Processing each item of a set efficiently. *)

  val map : unit Map.t t -> (Map.key -> 'b t -> changeable) -> 'b Map.t t
  (** [map x fn] applies [fn] to each element of [x] and returns a map from
      input elements to results. When new elements are added to [x], it only
      runs [fn] on the new elements, rather than on all elements. *)
end
