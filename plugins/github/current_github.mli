(** Integration with GitHub. *)

type t
(** Configuration for accessing GitHub. *)

module Repo_id : sig
  (** Identifies a repository hosted on GitHub. *)

  type t = private {
    owner : string;
    name : string;
  }

  val pp : t Fmt.t

  val cmdliner : t Cmdliner.Arg.conv
end

val v : token:string -> unit -> t
(** [v ~token ()] is a configuration that authenticates to GitHub using [token]. *)

val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
(** [exec_graphql t query] executes [query] on GitHub. *)

val head_commit : t -> Repo_id.t -> Current_git.Commit_id.t Current.t
(** [head_commit t repo] evaluates to the commit at the head of the default branch in [repo]. *)

val input_webhook : Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
(** Call this whenever an HTTP request is received on the web-hook endpoint. *)

val cmdliner : t Cmdliner.Term.t
(** Command-line options to generate a GitHub configuration. *)
