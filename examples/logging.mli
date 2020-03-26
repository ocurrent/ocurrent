val init : ?level:Logs.level -> unit -> unit
(** Initialise the Logs library with some sensible defaults. *)

val run : unit Current.or_error Lwt.t -> unit Current.or_error
(** [run x] is like [Lwt_main.run x], but logs the returned error, if any. *)
