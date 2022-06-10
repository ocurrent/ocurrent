(** Integration with GitLab {{:https://docs.gitlab.com/ee/user/project/integrations/webhooks.html}Webhooks}. *)

val webhook : webhook_secret:string -> Current_web.Resource.t
(** GitLab webhook endpoint.

To trigger events this MUST be added to {! Current_web.routes }. This webhook handles the events:

 - Merge request
 - Push

See {{:https://docs.gitlab.com/ee/user/project/integrations/webhook_events.html}webhook events} for
a full list of possible events.
*)

(** Identifier for a Project's repository hosted on GitLab. *)
module Repo_id : sig
  type t = { owner : string
           ; name : string
           ; project_id : int
           }

  val pp : t Fmt.t

  val compare : t -> t -> int

  val cmdliner : t Cmdliner.Term.t
  (** Cmdliner parser for reading a repo_id as a string.
      Expected format is "owner/name/project_id"
  *)
end

(** Access to the GitLab API. *)
module Api : sig
  type t
  (** Configuration for accessing GitLab. *)

  val webhook_secret : t -> string
  (** Webhook secret to validate payloads from GitLab. *)

  type refs
  (** Reference information for the Repository. *)

  (** Status associated with a single [Commit.t]. *)
  module Status : sig
    type t
    (** GitLab commit context status type. *)

    type state = [`Cancelled | `Failure | `Running | `Pending | `Success ]
    (** All possible Commit states in GitLab. *)

    val v : name:string -> ?description:string -> ?url:Uri.t -> state -> t
    (** Create [t] with an optional [?url] and [?description] to associate with the displayed status in GitLab.*)
  end

  (** A specific Git commit. *)
  module Commit : sig
    type t

    val id : t -> Current_git.Commit_id.t
    (** Identifier for a specific Git commit, which can be used to fetch it. *)

    val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
    (** [set_status commit context status] sets the status of [commit]/[context] to [status]. *)

    val owner_name : t -> string
    (** [owner_name t] is the "owner/name" string identifying the repository. *)

    val repo_id : t -> Repo_id.t
    (** Construct a [Repo_id.t] that [t] belongs to. *)

    val hash : t -> string
    (** [hash t] is the Git commit hash of [t]. *)

    val committed_date : t -> string
    (** [committed_date t] is the datetime when [t] was committed. *)

    val pp : t Fmt.t
    (** Pretty print [t]. *)

    val compare : t -> t -> int

    val uri : t -> Uri.t
    (** [uri t] is a URI for the GitLab web page showing [t]. *)
  end

  (** A Project's repository on GitLab. *)
  module Repo : sig
    type nonrec t = t * Repo_id.t

    val id : t -> Repo_id.t
    val pp : t Fmt.t
    val compare : t -> t -> int

    val ci_refs : ?staleness:Duration.t -> t Current.t -> Commit.t list Current.t
    (** [ci_refs t] evaluates to the list of branches and open PRs in [t].
        @param staleness If given, commits older than this are excluded.
                         Note: the main branch commit is always included, even if stale. *)

    val head_commit : t Current.t -> Commit.t Current.t
    (** [head_commit t] evaluates to the commit at the head of the default branch in [t]. *)
  end

  (** A Ref as an indirect way of referring to a commit. *)
  module Ref : sig
    type t = [ `Ref of string | `PR of int ]
    (** Ref is either a regular git ref or a MergeRequest. *)

    val pp : t Fmt.t
    val compare : t -> t -> int

    val to_git : t -> string
    (** [to_git t] is the Git-format string of the ref, e.g."refs/pull/%d/head" *)
  end

  module Ref_map : Map.S with type key = Ref.t

  val head_commit : t -> Repo_id.t -> Commit.t Current.t
  (** [head_commit t repo] evaluates to the commit at the head of the default branch in [repo]. *)

  val ci_refs' : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t Ref_map.t Current.t
  (** [ci_refs' t repo] evaluates to the list of branches and open PRs in [repo].
      @param staleness If given, commits older than this are excluded.
        Note: the main branch commit is always included, even if stale. *)

  val ci_refs : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t list Current.t
  (** [ci_refs t repo] evaluates to the list of branches and open PRs in [repo].
      @param staleness If given, commits older than this are excluded.
                       Note: the main branch commit is always included, even if stale. *)

  val refs : t -> Repo_id.t -> refs Current.Primitive.t
  (** [refs t repo] is the primitive for all the references in [repo].
      This is the low-level API for getting the refs.
      It is used internally by [ci_refs] and [head_of] but in some cases you may want to use it directly,
      [default_ref] and [all_refs] will expose useful information for you.
      The result is cached (so calling it twice will return the same primitive). *)

  val default_ref : refs -> Ref.t
  (** [default_ref refs] will return the full name of the repository's default branch ref *)

  val all_refs : refs -> Commit.t Ref_map.t
  (** [all_refs refs] will return a map of all the repository's refs *)

  (** Perform Anonymous request to GitLab. *)
  module Anonymous : sig
    val head_of : Repo_id.t -> Ref.t -> Current_git.Commit_id.t Current.t
    (** [head_of repo ref] is the head commit of [repo]/[ref]. No API token is used to access this,
        so it only works for public repositories. You are responsible for adding a web-hook so
        that [input_webhook] gets called whenever the commit changes. *)
  end

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitLab configuration. *)
end


(** Use GitLab to authenticate users. *)
module Auth : sig
  type t
  (** Configuration for GitLab OAuth single-sign-on. *)

  val v : ?scopes:string list -> client_id:string -> client_secret:string -> redirect_uri:string -> unit -> t
  (** Create a configuration using the details provided by GitLab. *)

  val make_login_uri : t -> csrf:string -> Uri.t
  (** Use this as your [~authn] in {!Current_web.Site.v}. *)

  val login : t option -> Current_web.Resource.t
  (** The callback page for logins. Add a route to this from the URL you
      configured when you set up your GitLab OAuth app.
      If [t = None] then the page will tell you how to configure it. *)

  val cmdliner : t option Cmdliner.Term.t
  (** Get the configuration from the command-line. *)

end
