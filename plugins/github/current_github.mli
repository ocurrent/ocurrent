(** Integration with GitHub. *)

val webhook : Current_web.Resource.t
(** Our web-hook endpoint. This must be added to {!Current_web.routes} so that we get notified of events. *)

module Repo_id : sig
  (** Identifies a repository hosted on GitHub. *)

  type t = {
    owner : string;
    name : string;
  }

  val pp : t Fmt.t

  val compare : t -> t -> int

  val cmdliner : t Cmdliner.Arg.conv
end

module Api : sig
  type t
  (** Configuration for accessing GitHub. *)

  module Status : sig
    type t
    (** GitHub commit context status type. *)

    type state = [`Error | `Failure | `Pending | `Success ]

    val v : ?description:string -> ?url:Uri.t -> state -> t
  end

  module Commit : sig
    type t

    val id : t -> Current_git.Commit_id.t
    (** The commit ID, which can be used to fetch it. *)

    val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
    (** [set_status commit context status] sets the status of [commit]/[context] to [status]. *)

    val owner_name : t -> string
    (** [owner_name t] is the "owner/name" string identifying the repository. *)

    val repo_id : t -> Repo_id.t
    (** Like [owner_name], but as a [Repo_id.t]. *)

    val hash : t -> string
    (** [hash t] is the Git commit hash of [t]. *)

    val committed_date : t -> string 
    (** [committed_date t] is the datetime when [t] was committed *)

    val pp : t Fmt.t
    val compare : t -> t -> int

    val uri : t -> Uri.t
    (** [uri t] is a URI for the GitHub web page showing [t]. *)
  end

  module Repo : sig
    type nonrec t = t * Repo_id.t

    val id : t -> Repo_id.t
    val pp : t Fmt.t
    val compare : t -> t -> int

    val ci_refs : t Current.t -> Commit.t list Current.t
    (** [ci_refs t] evaluates to the list of branches and open PRs in [t], excluding gh-pages. *)

    val head_commit : t Current.t -> Commit.t Current.t
    (** [head_commit t] evaluates to the commit at the head of the default branch in [t]. *)
  end

  module Ref : sig
    type t = [ `Ref of string | `PR of int ]

    val pp : t Fmt.t

    val compare : t -> t -> int

    val to_git : t -> string
    (** [to_git t] is the Git-format string of the ref, e.g."refs/pull/%d/head" *)
  end

  module Ref_map : Map.S with type key = Ref.t

  val of_oauth : string -> t
  (** [of_oauth token] is a configuration that authenticates to GitHub using [token]. *)

  val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
  (** [exec_graphql t query] executes [query] on GitHub. *)

  val head_commit : t -> Repo_id.t -> Commit.t Current.t
  (** [head_commit t repo] evaluates to the commit at the head of the default branch in [repo]. *)

  val head_of : t -> Repo_id.t -> Ref.t -> Commit.t Current.t
  (** [head_of t repo id] evaluates to the commit at the head of [id] in [repo].
      e.g. [head_of t repo (`Ref "refs/heads/master")] *)

  val ci_refs : ?staleness:(int option) -> t -> Repo_id.t -> Commit.t list Current.t
  (** [ci_refs t repo] evaluates to the list of branches and open PRs in [repo], excluding gh-pages.
      The optional [staleness] argument can be used to specify the number of days beyond which the latest
      commit of a PR or branch is deemed too old and will be removed. It defaults to [None] and supplying 
      a number less than [0] behaves like [None]. *)

  val refs : t -> Repo_id.t -> Commit.t Ref_map.t Current.Primitive.t
  (** [refs t repo] is the primitive for all the references in [repo].
      This is the low-level API for getting the refs.
      It is used internally by [ci_refs] and [head_of] but in some cases you may want to use it directly.
      The result is cached (so calling it twice will return the same primitive). *)

  module Anonymous : sig
    val head_of : Repo_id.t -> Ref.t -> Current_git.Commit_id.t Current.t
    (** [head_of repo ref] is the head commit of [repo]/[ref]. No API token is used to access this,
        so it only works for public repositories. You are responsible for adding a web-hook so
        that [input_webhook] gets called whenever the commit changes. *)
  end

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub configuration. *)
end

module Installation : sig
  type t
  (** Details about a specific installation of a GitHub app. *)

  val account : t -> string
  (** The GitHub account name (organisation or owner) for this installation. *)

  val api : t -> Api.t
  (** Provides access to the API as this installation. *)

  val pp : t Fmt.t
  (** The GitHub account that installed the app. *)

  val repositories : ?include_archived:bool -> t Current.t -> Api.Repo.t list Current.t
  (** [repositories t] evaluates to the list of repositories which the user
      configured for this installation.
      @param include_archived If [false] (the default) then filter out archived repositories. *)

  val compare : t -> t -> int
  (** Order by installation ID. *)
end

module App : sig
  type t
  (** Configuration for a GitHub application. *)

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub app configuration. *)

  val installation : t -> account:string -> int -> Installation.t
  (** [installation t ~account id] gives access to the API for installation [id].
      Note: this generates a fresh value on each call and should not be used inside
      a pipeline. It is intended to be called once from your [main] function, for
      apps with only a single installation.
      @param account The GitHub account that installed the application. *)

  val installations : t -> Installation.t list Current.t
  (** [installations t] evaluates to the list of installations for this app. *)
end

(** Use GitHub to authenticate users. *)
module Auth : sig
  type t
  (** Configuration for GitHub OAuth single-sign-on. *)

  val v : ?scopes:string list -> client_id:string -> client_secret:string -> unit -> t
  (** Create a configuration using the details provided by GitHub. *)

  val make_login_uri : t -> csrf:string -> Uri.t
  (** Use this as your [~authn] in {!Current_web.Site.v}. *)

  val login : t option -> Current_web.Resource.t
  (** The callback page for logins. Add a route to this from the URL you
      configured when you set up your GitHub OAuth app.
      If [t = None] then the page will tell you how to configure it. *)

  val cmdliner : t option Cmdliner.Term.t
  (** Get the configuration from the command-line. *)
end
