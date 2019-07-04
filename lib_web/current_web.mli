val run : ?mode:Conduit_lwt_unix.server -> Current.Engine.t -> unit Lwt.t
(** [run ~mode engine] runs a web-server (with configuration [mode]) showing the state of [engine]. *)

val cmdliner : Conduit_lwt_unix.server Cmdliner.Term.t
