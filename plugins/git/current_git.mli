(** Git plugin for OCurrent. *)

type commit = private string

val commit_of_string : string -> commit

val fetch : commit Current.t -> Fpath.t Current.t
(** [fetch commit] pulls [commit] to a local Git repository. *)
