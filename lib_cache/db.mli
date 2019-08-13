module Build : sig
  type entry = {
    job_id : string;
    build : int64;      (* Build number (increases for rebuilds). *)
    value : string Current.or_error;
    rebuild : bool;     (* If [true], then a rebuild was requested. *)
    finished : float;   (* When the entry was created. *)
  }

  val record :
    builder:string ->
    build:int64 ->
    key:string ->
    job:string ->
    ready:Unix.tm ->
    running:Unix.tm option ->
    finished:Unix.tm ->
    string Current.or_error ->
    unit
  (** [record ~builder ~build ~key ~log ~created ~ready ~finished value] stores [value] as the result of building [key] with [builder].
      This replaces any previous entry.
      @param log The ID for the log.
      @param ready When the job was ready to start (i.e. enqueued).
      @param running When the job started running.
      @param finished When the job stopped running (i.e. now).
  *)

  val lookup : builder:string -> string -> entry option
  (** [lookup ~builder key] returns the stored result for [builder] and [key], with the highest build number, if any. *)

  val drop_all : string -> unit
  (** [drop_all builder] drops all cached entries for [builder]. *)

  val invalidate : builder:string -> string -> unit
  (** [invalidate ~builder key] removes any existing entry for [builder, key]. *)

  val query : ?ok:bool -> unit -> entry list
end

module Publish : sig
  type entry = {
    job_id : string;
    value : string;
    outcome : string;
  }

  val record :
    op:string ->
    key:string ->
    value:string ->
    job_id:string ->
    string ->
    unit
  (** [record ~op ~key ~value ~job_id outcome] records that [key] is now set to [value],
      producing [outcome]. This replaces any previous entry. *)

  val lookup : op:string -> string -> entry option
  (** [lookup ~op key] returns the previously stored result for [op] and [key], if any. *)

  val drop_all : string -> unit
  (** [drop_all op] drops all cached entries for [op]. *)

  val invalidate : op:string -> string -> unit
  (** [invalidate ~op key] removes any existing entry for [op, key]. *)
end

