(* The runtime CI monad, similar to DataKitCI.
   Nothing interesting here. *)

type 'a t = ('a, Id.t * [`Active of Output.active | `Msg of string]) result
(** For the error case, the `Id.t` indicates the component which is active or failed.
    This is used to show downstream components as blocked. *)

val active : id:Id.t -> Output.active -> 'a t
val return : 'a -> 'a t
val fail : id:Id.t -> string -> 'a t
val state : 'a t -> ('a, [`Active of Output.active | `Msg of string]) result t
val catch : 'a t -> 'a S.or_error t
val map : id:Id.t -> ('a -> 'b) -> 'a t -> 'b t
val map_error : id:Id.t -> (string -> string) -> 'a t -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val pair : 'a t -> 'b t -> ('a * 'b) t

val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val pp : 'a Fmt.t -> 'a t Fmt.t

val run : 'a t -> 'a Output.t
