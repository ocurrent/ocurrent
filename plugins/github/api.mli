(* Public API; see Current_git.mli for details of these: *)

module Status : sig
  type t
  type state = [`Error | `Failure | `Pending | `Success ]

  val v : ?description:string -> ?url:Uri.t -> state -> t
end

module Commit : sig
  type t
  val id : t -> Current_git.Commit_id.t
  val owner_name : t -> string
  val hash : t -> string
  val pp : t Fmt.t
  val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
end

type t
val of_oauth : string -> t
val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
val head_commit : t -> Repo_id.t -> Commit.t Current.t
val head_commit_dyn : t Current.t -> Repo_id.t Current.t -> Commit.t Current.t
val ci_refs : t -> Repo_id.t -> Commit.t list Current.t
val ci_refs_dyn : t Current.t -> Repo_id.t Current.t -> Commit.t list Current.t
val cmdliner : t Cmdliner.Term.t

(* Private API *)

val read_file : string -> string
(** [read_file path] is the contents of the file at [path] (just a utility function; should be moved elsewhere). *)

type token = {
  token : (string, [`Msg of string]) result;
  (** A token to include in the "Authorization" header, or an error if we failed to get a token. *)

  expiry : float option;
  (** [token] is valid until this time.
      If [token] is an [Error] then this is the earliest time to try again.
      If [None], [token] does not expire. *)
}

val get_token : t -> (string, [`Msg of string]) result Lwt.t
(** [get_token t] returns the cached token for [t], or fetches a new one if it has expired. *)

val input_webhook : unit -> unit
(** Call this when we get a webhook event. *)

val v : get_token:(unit -> token Lwt.t) -> string -> t
(** [v ~get_token account] is a configuration that uses [get_token] when it needs to get or refresh the API token.
    Note: [get_token] can return a failed token, in which case the expiry time says when to try again.
          If [get_token] instead raises an exception, this is turned into an error token with a 1 minute expiry.
    @param account This is a string used to label point counters in Prometheus. *)
