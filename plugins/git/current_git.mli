(** Integration with Git. *)

module Commit_id : sig
  include Set.OrderedType

  val v : repo:string -> gref:string -> hash:string -> t
  (** [v ~repo ~gref ~hash] identifies a commit that can be fetched from [repo]
      using [gref] as the reference name and has hash [hash]. *)

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

module Commit : sig
  include Set.OrderedType

  val id : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
end

val fetch : Commit_id.t Current.t -> Commit.t Current.t

val with_checkout : Commit.t -> (Fpath.t -> 'a Current.or_error Lwt.t) -> 'a Current.or_error Lwt.t
(** [with_checkout c fn] clones [c] to a temporary directory and runs [fn tmpdir]. When it returns,
    the directory is deleted. *)

module Local : sig
  type t
  (** A local Git repository. *)

  val v : Fpath.t -> t
  (** [v path] is the local Git repository at [path]. *)

  val head : t -> string -> Commit_id.t Current.t
  (** [head t gref] evaluates to the commit at the head of [gref].
      e.g. [head t "/refs/heads/master"] *)
end
