(** Git plugin for OCurrent. *)

module Commit : sig
  type t
  val v : repo:string -> hash:string -> t
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val digest : t -> string
end

val complete_clone : Commit.t -> unit
(** Test function that records a clone as completed. *)

val reset : unit -> unit
(** Reset all repositories to uncloned. *)

val fetch : Commit.t Current.t -> Fpath.t Current.t
(** [fetch commit] pulls [commit] to a local Git repository. *)
