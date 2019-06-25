val record : builder:string -> key:string -> log:string -> (string, [`Msg of string]) result -> unit
(** [record ~builder ~key ~log value] stores [value] as the result of building [key] with [builder].
    This replaces any previous entry.
    @param log The ID for the log. *)

val lookup : builder:string -> string -> (string, [`Msg of string]) result option
(** [lookup ~builder key] returns the previously stored result for [builder] and [key], if any. *)

val drop_all : unit -> unit
(** [drop_all ()] drops all cached entries. *)
