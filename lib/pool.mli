type t

val create : label:string -> int -> t

val get : on_cancel:((string -> unit Lwt.t) -> unit Lwt.t) -> switch:Switch.t -> t -> unit Lwt.t
(** [get ~on_cancel ~switch t] waits for a resource and then returns.
    The resource will be returned to the pool when [switch] is turned off. *)

val pp : t Fmt.t
