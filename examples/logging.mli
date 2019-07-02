val init : unit -> unit
(** Initialise the Logs library with some sensible defaults. *)

val with_dot :
  dotfile:Fpath.t ->
  (unit -> 'a Current.t) ->
  (unit -> 'a Current.t)
(** [with_dot ~dotfile pipeline] wraps [pipeline] to keep a dot diagram in
    [dotfile] showing its current state. *)
