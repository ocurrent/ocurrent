(* Static analysis of a build pipeline. *)

module Make (Job : sig type id end) : sig
  type t

  type state =
    | Blocked
    | Active of Output.active
    | Pass
    | Fail of string

  type env = t option
  (** When evaluating a bind function we need to record that everything also
      depends on the bind itself, so all the constructors take this in an
      [~env] argument. This will be [None] if the node wasn't created by
      a bind. *)

  val return       : env:env -> string option -> t
  val fail         : env:env -> string -> t
  val map_input    : env:env -> t -> (string, [`Blocked | `Empty_list]) result -> t
  val map_failed   : env:env -> t -> string -> t
  val option_input : env:env -> t -> [`Blocked | `Selected | `Not_selected] -> t
  val state        : env:env -> hidden:bool -> t -> t
  val catch        : env:env -> hidden:bool -> t -> t
  val active       : env:env -> Output.active -> t
  val of_output    : env:env -> _ Output.t -> t
  val pair         : env:env -> t -> t -> t
  val bind         : env:env -> ?info:string -> t -> state -> t
  val bind_input   : env:env -> info:string -> ?id:Job.id -> t -> state -> t
  val list_map     : env:env -> f:t -> t -> t
  val option_map   : env:env -> f:t -> t -> t
  val gate         : env:env -> on:t -> t -> t

  val booting : t

  val job_id : t -> Job.id option

  val stats : t -> S.stats

  val pp : t Fmt.t
  val pp_dot : url:(Job.id -> string option) -> t Fmt.t
end
