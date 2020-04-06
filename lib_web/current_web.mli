module Resource : sig
  (* A single HTTP resource in the web UI. *)
  class virtual t : object
    method private get : Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle GET requests.
        The default method returns a [`Bad_request] error. *)

    method private post : Cohttp.Request.t -> string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle POSTs.
        The default method returns a [`Bad_request] error. *)

    method get_raw : Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP GET request. The default method calls [get]. *)

    method post_raw : Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP POST request.
        The default method reads the body, checks the CSRF token, and then calls [post]. *)
  end
end

val routes : Current.Engine.t -> Resource.t Routes.route list
(** [routes engine] is the default routes for a web interface to [engine]. *)

val run : ?mode:Conduit_lwt_unix.server -> Resource.t Routes.route list -> ('a, [`Msg of string]) result Lwt.t
(** [run ~mode routes] runs a web-server (with configuration [mode]) that handles incoming requests with [routes]. *)

val cmdliner : Conduit_lwt_unix.server Cmdliner.Term.t
