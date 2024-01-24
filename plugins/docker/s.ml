type source = [
  | `No_context
  | `Dir of Fpath.t Current.t
  | `Git of Current_git.Commit.t Current.t
]

type repo_id = string

module type DOCKER = sig
  module Image : sig
    include Current_cache.S.WITH_DIGEST
    include Current_cache.S.WITH_MARSHAL with type t := t

    val of_hash : string -> t
    val hash : t -> string
    val pp : t Fmt.t
  end

  val docker_context : string option

  val pull :
    ?auth:(string * string) ->
    ?label:string ->
    ?arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> Image.t Current.t
  (** [pull ~schedule tag] ensures that the latest version of [tag] is cached locally, downloading it if not.
      @param arch Select a specific architecture from a multi-arch manifest.
      @param schedule Controls how often we check for updates. If the schedule
                      has no [valid_for] limit then we will only ever pull once.
      @param auth If given, do a "docker login" using this username/password pair before pulling.
                  If username is in the form "user@host", the registry at "host" will be used *)

  val peek :
    ?label:string ->
    arch:string ->
    schedule:Current_cache.Schedule.t ->
    string -> repo_id Current.t
  (** [peek ~schedule ~arch tag] gets the latest version of [tag] without actually pulling it.
      @param arch Select a specific architecture from a multi-arch manifest.
      @param schedule Controls how often we check for updates. If the schedule
                      has no [valid_for] limit then we will only ever check once. *)

  val build :
    ?level:Current.Level.t ->
    ?schedule:Current_cache.Schedule.t ->
    ?timeout:Duration.t ->
    ?squash:bool ->
    ?buildx:bool ->
    ?label:string ->
    ?dockerfile:[`File of Fpath.t | `Contents of string] Current.t ->
    ?path:Fpath.t ->
    ?pool:unit Current.Pool.t ->
    ?build_args:string list ->
    pull:bool ->
    source ->
    Image.t Current.t
  (** [build ~pull src] builds a Docker image from source.
      @param timeout If set, abort builds that take longer than this.
      @param squash If set to [true], pass "--squash" to "docker build".
      @param buildx If set to [true], runs with "docker buildx build" instead of "docker build".
      @param dockerfile If present, this is used as the contents of the Dockerfile.
      @param pull If [true], always check for updates and pull the latest version.
      @param pool Rate limit builds by requiring a resource from the pool.
      @param path The relative file path passed to the docker build command as the build context.
                  No checks are done over the path: it can point anywhere outside the build directory. *)

  val run :
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    unit Current.t
  (** [run image ~args] runs [image args] with Docker.
      @param run_args List of additional arguments to pass to the "docker
                      run" subcommand. *)

  val pread :
    ?label:string ->
    ?pool:unit Current.Pool.t ->
    ?run_args:string list ->
    Image.t Current.t -> args:string list ->
    string Current.t
  (** [pread image ~args] runs [image args] with Docker the same way than [run]
      does but returns its stdout as a string. *)

  val tag : tag:string -> Image.t Current.t -> unit Current.t
  (** [tag image ~tag] does "docker tag image tag" *)

  val push : ?auth:(string * string) -> tag:string -> Image.t Current.t -> repo_id Current.t
  (** [push image ~tag] does "docker tag image tag && docker push tag".
      @param auth If given, do a "docker login" using this username/password pair before pushing.
                  If username is in the form "user@host", the registry at "host" will be used *)

  val service : name:string -> image:Image.t Current.t -> unit -> unit Current.t
  (** [service ~name ~image ()] keeps a Docker SwarmKit service up-to-date. *)

  val compose : ?pull:bool -> name:string -> contents:string Current.t -> unit -> unit Current.t
  (** [compose ?pull ~name ~image ~contents ()] keeps a Docker Compose deployment up-to-date.
      [contents] contains the full Compose Yaml file.
      @param pull Controls whether images are pulled by the compose command, the default is [true] 
      This calls `docker-compose` version 1 which as of April 2022 is deprecated in favour of version 2 *)

  val compose_cli :
    ?pull:bool ->
    ?up_args: string list ->
    name:string ->
    detach:bool ->
    contents:string Current.t ->
    unit -> unit Current.t
  (** [compose_cli ~name ~image ~contents ()] keeps a Docker Compose Cli deployment up-to-date.
      [contents] contains the full Compose Yaml file.
      [up_args] contains additional arguments to pass to the {e docker compose up} command.
      This calls {e docker compose} which is GA as of April 2022 and should be used in preference over version 1. *)
end

module type HOST = sig
  val docker_context : string option
  (** The value to pass to Docker via the "--context" argument ([None] for no argument). *)
end
