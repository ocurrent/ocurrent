(* The main API exposed to users. *)

type +'a t
(** An ['a t] is a pipeline that produces a value of type ['a]. *)

val pending : 'a t
(** [pending] is a pipeline that never produces a result. *)

val return : 'a -> 'a t
(** [return x] is a pipeline that immediately succeeds with [x]. *)

val fail : string -> 'a t
(** [fail m] is a pipeline that immediately fails with message [m]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
val pair : 'a t -> 'b t -> ('a * 'b) t
val bind : ?name:string -> ('a -> 'b t) -> 'a t -> 'b t

val list_map : ('a t -> 'b t) -> 'a list t -> 'b list t
(** [list_map f xs] adds [f] to the end of each input pipeline
    and collects all the results into a single list. *)

val list_iter : ('a t -> unit t) -> 'a list t -> unit t
(** Like [list_map] but for the simpler case when the result is unit. *)

val all : unit t list -> unit t
(** [all xs] is a pipeline that succeeds if every pipeline in [xs] succeeds. *)

val gate : on:unit t -> 'a t -> 'a t
(** [gate ~on:ctrl x] is the same as [x], once [ctrl] succeeds. *)

val pp : 'a t Fmt.t
val pp_dot : 'a t Fmt.t

type 'a output = ('a, [`Pending | `Msg of string]) result

val pp_output : 'a Fmt.t -> 'a output Fmt.t

val run : 'a t -> 'a output
val analyse : 'a t -> Static.t

module Syntax : sig
  val (let+) : 'a t -> ('a -> 'b) -> 'b t
  val (and+) : 'a t -> 'b t -> ('a * 'b) t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let**) : 'a t -> ('a -> 'b t) -> string -> 'b t
  val (and*) : 'a t -> 'b t -> ('a * 'b) t
end
