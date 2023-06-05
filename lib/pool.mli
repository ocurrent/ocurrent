type 'a t

type priority = [ `High | `Low ]

val create : label:string -> int -> unit t

val of_fn :
  label : string ->
  (sw:Eio.Switch.t -> priority:priority -> 'a Eio.Promise.or_exn * (unit -> unit)) ->
  'a t

val get :
  'a t ->
  sw:Eio.Switch.t ->
  priority:priority ->
  'a Eio.Promise.or_exn * (unit -> unit)
(** [get ~priority ~switch t] waits for a resource and then returns.
    It also returns a function that can be used to cancel the request.
    The resource will be returned to the pool when [switch] is turned off. *)

val pp : _ t Fmt.t
