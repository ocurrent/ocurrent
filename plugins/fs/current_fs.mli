val save : Fpath.t Current.t -> string Current.t -> unit Current.t
(** [save path value] ensures that the path [path] contains [value], updating it
    atomically if not. *)

module File : sig
  val read : Fpath.t -> string Current.t
  (** [read path] reads the contents of [path] returning the result of the read *)

  val write : Fpath.t -> string Current.t -> unit Current.t
  (** [write path content] will write [content] to [path] *)
end

module Dir : sig
  val contents : ?recursive:bool -> Fpath.t -> Fpath.t list Current.t
  (** [contents ?recursive dir] will give you the contents of [dir], if [recursive]
      is [true] any directories will be recursively searched for contents too. *)
end