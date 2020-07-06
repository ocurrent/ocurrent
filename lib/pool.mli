type 'a t

type priority = [ `High | `Low ]

val create : label:string -> int -> unit t

val of_fn :
  label : string ->
  (priority:priority -> switch:Switch.t -> 'a Lwt.t * (unit -> unit Lwt.t)) ->
  'a t

val get :
  'a t ->
  priority:priority ->
  switch:Switch.t ->
  'a Lwt.t * (unit -> unit Lwt.t)
(** [get ~priority ~switch t] waits for a resource and then returns.
    It also returns a function that can be used to cancel the request.
    The resource will be returned to the pool when [switch] is turned off. *)

val pp : _ t Fmt.t
