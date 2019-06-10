(* Some dummy build primitives. *)

type source = Fpath.t
type image

val build : ?on:string -> source Current.t -> image Current.t
(** [build ~on:platform src] builds a Docker image from source. *)

val run : image Current.t -> cmd:string list -> unit Current.t
(** [run image ~cmd] runs [cmd] in Docker image [image]. *)

val complete : string -> cmd:string list -> [`Failed | `Complete] -> unit
(** Marks a previous [run] as complete. *)

val push : image Current.t -> tag:string -> unit Current.t
(** [push x ~tag] publishes [x] on Docker Hub as [tag]. *)

val reset : unit -> unit
(** Reset state for tests. *)

val assert_finished : unit -> unit
(** Check all containers are stopped. *)
