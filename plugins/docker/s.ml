type source = Current_git.Commit.t

module type DOCKER = sig
  module Image : sig
    type t
    val hash : t -> string
  end

  val pull : schedule:Current_cache.Schedule.t -> string -> Image.t Current.t
  (** [pull ~schedule tag] ensures that the latest version of [tag] is cached locally, downloading it if not.
      @param schedule Controls how often we check for updates. If the schedule
                      has no [valid_for] limit then we will only ever pull once. *)

  val build : ?label:string -> ?dockerfile:Dockerfile.t Current.t -> pull:bool -> source Current.t -> Image.t Current.t
  (** [build ~pull src] builds a Docker image from source.
      @param dockerfile If present, this is used as the contents of the Dockerfile.
      @param pull If [true], always check for updates and pull the latest version. *)

  val run : Image.t Current.t -> args:string list -> unit Current.t
  (** [run image ~args] runs [image args] with Docker. *)

  val tag : tag:string -> Image.t Current.t -> unit Current.t
  (** [tag image ~tag] does "docker tag image tag" *)

  val push : tag:string -> Image.t Current.t -> unit Current.t
  (** [push image ~tag] does "docker tag image tag && docker push tag" *)
end

module type HOST = sig
  val docker_host : string option
  (** The value to pass to Docker via the "-H" argument ([None] for no argument). *)
end
