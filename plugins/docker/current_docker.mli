(** Integration with Docker containers. *)

module S = S

module Default : S.DOCKER
(** The default Docker engine (from the [$DOCKER_HOST] environment variable). *)

module Make (_ : S.HOST) : S.DOCKER
(** The docker engine running on [Host]. *)

val push_manifest : ?auth:(string * string) -> tag:string -> S.repo_id Current.t list -> S.repo_id Current.t
(** [push_manifest images ~tag] pushes a manifest containing [images] as [tag].
    @param auth If give, do a "docker login" using this username/password pair before pushing. *)

(** Low-level API. This is useful for building custom components.
    The functions are similar to the ones in {!S.DOCKER} except that each one
    takes the context explicitly as an argument, there are no labels, input
    values are no longer wrapped with [Current.t], and the output is a
    {!Current.Primitive.t} type. You can wrap these with [let>] to create
    your own components. *)
module Raw : sig
  module Image = Image

  val pull :
    docker_context:string option ->
    schedule:Current_cache.Schedule.t ->
    ?auth:(string * string) ->
    ?arch:string -> string -> Image.t Current.Primitive.t

  val peek :
    docker_context:string option ->
    schedule:Current_cache.Schedule.t ->
    arch:string -> string -> S.repo_id Current.Primitive.t

  val build :
    docker_context:string option ->
    ?level:Current.Level.t ->
    ?schedule:Current_cache.Schedule.t ->
    ?timeout:Duration.t ->
    ?squash:bool ->
    ?buildx:bool ->
    ?dockerfile:[`File of Fpath.t | `Contents of string] ->
    ?path:Fpath.t ->
    ?pool:unit Current.Pool.t ->
    ?build_args:string list ->
    pull:bool ->
    [ `Git of Current_git.Commit.t | `Dir of Fpath.t | `No_context ] ->
    Image.t Current.Primitive.t

  val run :
    docker_context:string option ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t -> args:string list ->
    unit Current.Primitive.t

  val pread :
    docker_context:string option ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t -> args:string list ->
    string Current.Primitive.t

  val tag :
    docker_context:string option ->
    tag:string -> Image.t -> unit Current.Primitive.t

  val push :
    docker_context:string option ->
    ?auth:(string * string) -> tag:string -> Image.t -> S.repo_id Current.Primitive.t

  val service :
    docker_context:string option ->
    name:string -> image:Image.t -> unit -> unit Current.Primitive.t

  val compose :
    ?pull:bool ->
    docker_context:string option ->
    name:string ->
    contents:string -> unit -> unit Current.Primitive.t

  val compose_cli :
    ?pull:bool ->
    ?up_args: string list ->
    docker_context:string option ->
    name:string ->
    detach:bool ->
    contents:string ->
    unit -> unit Current.Primitive.t

  (** Building Docker commands. *)
  module Cmd : sig
    type t = Lwt_process.command

    val docker : string list -> docker_context:string option -> t
    (** [docker ~docker_context args] is a command to run docker, with the "--context" argument added (if necessary).
        e.g. [docker ~docker_context ["run"; image]] *)

    val with_container :
      docker_context:string option ->
      kill_on_cancel:bool ->
      job:Current.Job.t ->
      t ->
      (string -> 'a Current.or_error Lwt.t) ->
      'a Current.or_error Lwt.t
    (** [with_container ~kill_on_cancel ~job t fn] runs [t] to create a new
        container (the output is the container ID), then calls [fn id].
        When [fn] returns, it removes the container (killing it first if necessary).
        If [fn] raises an exception, it catches it and turns it into an error return.
        @param kill_on_cancel "docker kill" the container if the the job is cancelled. *)

    val pp : t Fmt.t
  end
end
