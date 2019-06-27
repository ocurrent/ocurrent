(** Integration with Docker containers. *)

type source = Current_git.Commit.t

module Image : sig
  type t
  val tag : t -> string
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
