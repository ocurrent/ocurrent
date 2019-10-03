(** An ANSI escape sequence parser. *)

type t

val create : unit -> t
(** Create a new parser in the default state. *)

val process : t -> string -> string
(** [process t data] reads in [data] and generates HTML output, parsing escape sequences as it goes.
    If [data] ends with a partial sequence, it remembers this and will resume processing on the
    next call to [process]. *)

val css : string
(** Some default CSS rules to make the HTML output appear in colour. *)
