(* Static analysis of a build pipeline. *)

type t

type state =
  | Blocked
  | Active
  | Pass
  | Fail

type env

val make_env : unit -> env
(** All linked static values must be created within the same environment (this is
    used to number them). *)

val with_bind : t -> env -> env
(* [with_bind b ctx] is the environment to use when evaluating the function
   passed to a [bind]. All static values created within this environment get an
   implicit dependency on [b]. *)

val return    : env:env -> unit -> t
val fail      : env:env -> unit -> t
val state     : env:env -> t -> t
val catch     : env:env -> t -> t
val pending   : env:env -> unit -> t
val of_output : env:env -> _ Output.t -> t
val pair      : env:env -> t -> t -> t
val bind      : env:env -> name:string -> t -> state -> t
val list_map  : env:env -> f:t -> t -> t
val gate      : env:env -> on:t -> t -> t

val booting : t

val set_state : t -> state -> unit

val pp : t Fmt.t
val pp_dot : t Fmt.t
