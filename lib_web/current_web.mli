type webhook = string * (Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
(** A webhook is a [(name, handler)] pair. *)

val run : ?mode:Conduit_lwt_unix.server -> ?webhooks:webhook list -> Current.Engine.t -> ('a, [`Msg of string]) result Lwt.t
(** [run ~mode engine] runs a web-server (with configuration [mode]) showing the state of [engine].
    @param webhooks A list of [name, handler] pairs. Requests to ["/webhook/$name"] are sent to [handler]. *)

val cmdliner : Conduit_lwt_unix.server Cmdliner.Term.t
