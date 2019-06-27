type entry = {
  value : string Current.or_error;
  finished : float;   (* When the entry was created. *)
}

val record :
  builder:string ->
  key:string ->
  log:string ->
  ready:Unix.tm ->
  running:Unix.tm option ->
  finished:Unix.tm ->
  string Current.or_error ->
  unit
(** [record ~builder ~key ~log ~created ~ready ~finished value] stores [value] as the result of building [key] with [builder].
    This replaces any previous entry.
    @param log The ID for the log.
    @param ready When the job was ready to start (i.e. enqueued).
    @param running When the job started running.
    @param finished When the job stopped running (i.e. now).
*)

val lookup : builder:string -> string -> entry option
(** [lookup ~builder key] returns the previously stored result for [builder] and [key], if any. *)

val drop_all : string -> unit
(** [drop_all builder] drops all cached entries for [builder]. *)

val invalidate : builder:string -> string -> unit
(** [invalidate ~builder key] removes any existing entry for [builder, key]. *)
