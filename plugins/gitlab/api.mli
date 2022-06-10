module Status : sig
  type t
  type state = [ `Cancelled | `Failure | `Pending | `Running | `Success ]
  val v : name:string -> ?description:string -> ?url:Uri.t -> state -> t
end

module Commit : sig
  type t
  val id : t -> Current_git.Commit_id.t
  val repo_id : t -> Repo_id.t
  val owner_name : t -> string
  val hash : t -> string
  val committed_date : t -> string
  val pp : t Fmt.t
  val compare : t -> t -> int
  val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
  val uri : t -> Uri.t
end

module Ref : sig
  type t = [ `PR of int | `Ref of string ]
  val compare : t -> t -> int
  val pp : t Fmt.t
  val to_git : t -> string
end

module Ref_map : Map.S with type key = Ref.t

type t
type refs

val of_oauth : token:string -> webhook_secret:string -> t
val head_commit : t -> Repo_id.t -> Commit.t Current.t
val refs : t -> Repo_id.t -> refs Current.Primitive.t
val default_ref : refs -> Ref.t
val webhook_secret : t -> string
val all_refs : refs -> Commit.t Ref_map.t
val ci_refs' : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t Ref_map.t Current.t
val ci_refs : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t list Current.t
val cmdliner : t Cmdliner.Term.t
val webhook_secret_file : string Cmdliner.Term.t

module Repo : sig
  type nonrec t = t * Repo_id.t

  val id : t -> Repo_id.t
  val ci_refs : ?staleness:Duration.t -> t Current.t -> Commit.t list Current.t
  val head_commit : t Current.t -> Commit.t Current.t
  val pp : t Fmt.t
  val compare : t -> t -> int
end

module Anonymous : sig
  val head_of : Repo_id.t -> Ref.t -> Current_git.Commit_id.t Current.t
end

(* Private API *)
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

type webhooks_accepted = [
  | `MergeRequest of Gitlab_t.merge_request_webhook
  | `Push of Gitlab_t.push_webhook
  ]

val input_webhook : webhooks_accepted -> unit
(** [input_webhook] is called when a [webhook_accepted] request is made. *)

val v : get_token:(unit -> token Lwt.t) -> account:string -> webhook_secret:string -> unit -> t
