type t

val create : label:string -> int -> t

val get : switch:Switch.t -> t -> unit Lwt.t
(** [get ~switch t] waits for a resource and then returns.
    The resource will be returned to the pool when [switch] is turned off. *)

val pp : t Fmt.t
