type source = [
  | `No_context
  | `Git of Current_git.Commit.t Current.t
]

type repo_id = string

module type DOCKER = sig
  module Image : sig
    include Current_cache.S.WITH_DIGEST
    include Current_cache.S.WITH_MARSHAL with type t := t

    val hash : t -> string
    val pp : t Fmt.t
  end

  val docker_context : string option

  val pull : ?label:string -> schedule:Current_cache.Schedule.t -> string -> Image.t Current.t
  (** [pull ~schedule tag] ensures that the latest version of [tag] is cached locally, downloading it if not.
      @param schedule Controls how often we check for updates. If the schedule
                      has no [valid_for] limit then we will only ever pull once. *)

  val build :
    ?schedule:Current_cache.Schedule.t ->
    ?timeout:Duration.t ->
    ?squash:bool ->
    ?label:string ->
    ?dockerfile:[`File of Fpath.t | `Contents of Dockerfile.t] Current.t ->
    ?pool:Current.Pool.t ->
    ?build_args:string list ->
    pull:bool ->
    source ->
    Image.t Current.t
  (** [build ~pull src] builds a Docker image from source.
      @param timeout If set, abort builds that take longer than this.
      @param squash If set to [true], pass "--squash" to "docker build".
      @param dockerfile If present, this is used as the contents of the Dockerfile.
      @param pull If [true], always check for updates and pull the latest version.
      @param pool Rate limit builds by requiring a resource from the pool. *)

  val run :
    ?label:string ->
    ?pool:Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    unit Current.t
  (** [run image ~args] runs [image args] with Docker.
      @param run_args List of additional arguments to pass to the "docker
                      run" subcommand. *)

  val pread :
    ?label:string ->
    ?pool:Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    string Current.t
  (** [pread image ~args] runs [image args] with Docker the same way than [run]
      does but returns its stdout as a string. *)

  val tag : tag:string -> Image.t Current.t -> unit Current.t
  (** [tag image ~tag] does "docker tag image tag" *)

  val push : ?auth:(string * string) -> tag:string -> Image.t Current.t -> repo_id Current.t
  (** [push image ~tag] does "docker tag image tag && docker push tag".
      @param auth If give, do a "docker login" using this username/password pair before pushing. *)

  val service : name:string -> image:Image.t Current.t -> unit -> unit Current.t
  (** [service ~name ~image ()] keeps a Docker SwarmKit service up-to-date. *)
end

module type HOST = sig
  val docker_context : string option
  (** The value to pass to Docker via the "--context" argument ([None] for no argument). *)
end
