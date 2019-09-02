(** Integration with GitHub. *)

val input_webhook : Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
(** Call this whenever an HTTP request is received on the web-hook endpoint. *)

module Repo_id : sig
  (** Identifies a repository hosted on GitHub. *)

  type t = private {
    owner : string;
    name : string;
  }

  val pp : t Fmt.t

  val cmdliner : t Cmdliner.Arg.conv
end

module Api : sig
  type t
  (** Configuration for accessing GitHub. *)

  type status = [`Error | `Failure | `Pending | `Success ]
  (** GitHub commit context status type. *)

  module Commit : sig
    type t

    val id : t -> Current_git.Commit_id.t
    (** The commit ID, which can be used to fetch it. *)

    val set_status : t Current.t -> string -> status Current.t -> unit Current.t
    (** [set_status commit context status] sets the status of [commit]/[context] to [status]. *)
  end

  val of_oauth : string -> t
  (** [of_oauth token] is a configuration that authenticates to GitHub using [token]. *)

  val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
  (** [exec_graphql t query] executes [query] on GitHub. *)

  val head_commit : t -> Repo_id.t -> Commit.t Current.t
  (** [head_commit t repo] evaluates to the commit at the head of the default branch in [repo]. *)

  val head_commit_dyn : t Current.t -> Repo_id.t Current.t -> Commit.t Current.t
  (** Like [head_commit], but the inputs are both currents. *)

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub configuration. *)
end

module Installation : sig
  type t
  (** Details about a specific installation of a GitHub app. *)

  val api : t -> Api.t
  (** Provides access to the API as this installation. *)

  val pp : t Fmt.t
  (** The GitHub account that installed the app. *)

  val repositories : t Current.t -> Repo_id.t list Current.t
  (** [repositories t] evaluates to the list of repositories which the user
      configured for this installation. *)
end

module App : sig
  type t
  (** Configuration for a GitHub application. *)

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub app configuration. *)

  val installation : t -> account:string -> int -> Installation.t
  (** [installation t ~account id] gives access to the API for installation [id].
      @param account The GitHub account that installed the application. *)

  val installations : t -> Installation.t list Current.t
  (** [installations t] evaluates to the list of installations for this app. *)
end
