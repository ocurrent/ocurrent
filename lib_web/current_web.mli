val run : ?mode:Conduit_lwt_unix.server -> Current.Engine.t -> ('a, [`Msg of string]) result Lwt.t
(** [run ~mode engine] runs a web-server (with configuration [mode]) showing the state of [engine]. *)

val cmdliner : Conduit_lwt_unix.server Cmdliner.Term.t
