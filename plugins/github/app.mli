val input_installation_webhook : unit -> unit
(** Call this when we get the "installation" event. *)

(* Public API; see Current_git.mli for details of these: *)

type t

val webhook_secret : t -> string
val cmdliner : t Cmdliner.Term.t
val cmdliner_opt : t option Cmdliner.Term.t
val installation : t -> account:string -> int -> Installation.t
val installations : t -> Installation.t list Current.t
