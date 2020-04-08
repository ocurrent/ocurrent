module Site : sig
  type t
  (** Site configuration settings. *)

  val v : ?name:string -> unit -> t
  (** [v ~name () is a site named [name] (used for the HTML title, etc). *)
end

module Context : sig
  type t
  (** The context of a single web request. *)

  val request : t -> Cohttp.Request.t

  val respond_ok : Context.t -> [< Html_types.div_content_fun ] Tyxml.Html.elt list -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_ok ctx content] returns a successful page with [content] inserted into the site template. *)

  val respond_redirect : Context.t -> Uri.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_redirect ctx uri] redirects the user to [uri]. *)

  val respond_error : Context.t -> Cohttp.Code.status_code -> string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_error ctx code msg] returns an error message to the user, inside the site template. *)
end

module Resource : sig
  (* A single HTTP resource in the web UI. *)
  class virtual t : object
    method private get : Context.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle GET requests.
        The default method returns a [`Bad_request] error. *)

    method private post : Context.t -> string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle POSTs.
        The default method returns a [`Bad_request] error. *)

    method get_raw : Site.t -> Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP GET request. *)

    method post_raw : Site.t -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP POST request.
        The default method reads the body, checks the CSRF token, and then calls [post]. *)
  end
end

val routes : Current.Engine.t -> Resource.t Routes.route list
(** [routes engine] is the default routes for a web interface to [engine]. *)

val run : ?mode:Conduit_lwt_unix.server -> site:Site.t -> Resource.t Routes.route list -> ('a, [`Msg of string]) result Lwt.t
(** [run ~mode ~site routes] runs a web-server (with configuration [mode]) that handles incoming requests with [routes]. *)

val cmdliner : Conduit_lwt_unix.server Cmdliner.Term.t
