(** Integration with Docker containers. *)

type source = Fpath.t
type image

val build : source Current.t -> image Current.t
(** [build src] builds a Docker image from source. *)
