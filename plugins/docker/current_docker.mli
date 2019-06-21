(** Integration with Docker containers. *)

type source = Current_git.Commit.t

module Image : sig type t end

val build : source Current.t -> Image.t Current.t
(** [build src] builds a Docker image from source. *)

val run : Image.t Current.t -> args:string list -> unit Current.t
(** [run image ~args] runs [image args] with Docker. *)
