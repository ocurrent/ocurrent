(** Git plugin for OCurrent. *)

type commit

val commit : repo:string -> hash:string -> commit

val complete_clone : commit -> unit
(** Test function that records a clone as completed. *)

val reset : unit -> unit
(** Reset all repositories to uncloned. *)

val fetch : commit Current.t -> Fpath.t Current.t
(** [fetch commit] pulls [commit] to a local Git repository. *)
