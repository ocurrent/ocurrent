(* The runtime CI monad, similar to DataKitCI.
   Nothing interesting here. *)

type 'a or_error = ('a, [`Pending | `Msg of string]) result

type +'a t

val pending : 'a t
val return : 'a -> 'a t
val fail : string -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val pair : 'a t -> 'b t -> ('a * 'b) t
val pp : 'a Fmt.t -> 'a or_error Fmt.t

val run : 'a t -> 'a or_error
