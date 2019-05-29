(* Static analysis of a build pipeline. *)

type t

val return : unit -> t
val fail : unit -> t
val pending : t
val pair : t -> t -> t
val bind : name:string -> t -> t
val list_map : f:t -> t -> t
val gate : on:t -> t -> t

val with_context : t -> ('a -> 'b) -> 'a -> 'b
(** [with_context ctx f x] runs [f x] with the global context set to [ctx].
    Every [t] created during this time will have this context attached to it.
    This is used to implement monadic binds, so that everything inside the
    bind gets an extra dependency on the bind itself. *)

val pp : t Fmt.t
val pp_dot : t Fmt.t
