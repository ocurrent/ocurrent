(* Static analysis of a build pipeline. *)

module Make (Job : sig type id end) : sig
  type t

  type state =
    | Blocked
    | Active of Output.active
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

  val blocked    : env:env -> unit -> t
  val return     : env:env -> string option -> t
  val fail       : env:env -> unit -> t
  val state      : env:env -> t -> t
  val catch      : env:env -> t -> t
  val active     : env:env -> Output.active -> t
  val of_output  : env:env -> _ Output.t -> t
  val pair       : env:env -> t -> t -> t
  val bind       : env:env -> ?info:string -> t -> state -> t
  val bind_input : env:env -> info:string -> t -> state -> t
  val list_map   : env:env -> f:t -> t -> t
  val gate       : env:env -> on:t -> t -> t

  val booting : t

  val set_state : t -> ?id:Job.id -> state -> unit

  val pp : t Fmt.t
  val pp_dot : url:(Job.id -> string option) -> t Fmt.t
end
