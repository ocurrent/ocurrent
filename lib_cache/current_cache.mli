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
  val get : ?schedule:Schedule.t -> B.t -> B.Key.t -> B.Value.t Current.t
  (** [get b k] is a term for the result of building [k]. *)

  val invalidate : B.Key.t -> unit
  (** [invalidate key] removes key from the cache. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Output (P : S.PUBLISHER) : sig
  val set : P.t -> P.Key.t -> P.Value.t -> unit Current.t
  (** [set p k v] is a term for the result of setting [k] to [v]. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

(**/**)

(* For unit tests we need our own test clock: *)

val timestamp : (unit -> float) ref
val sleep : (float -> unit Lwt.t) ref
