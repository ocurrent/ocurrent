val save : Fpath.t Current.t -> string Current.t -> unit Current.t
(** [save path value] ensures that the path [path] contains [value], updating it
    atomically if not. *)
