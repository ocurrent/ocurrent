type entry = {
  job_id : string;
  build : int64;      (* Build number (increases for rebuilds). *)
  value : string;
  outcome : string Current.or_error;
  ready : float;
  running : float option;
  finished : float;
  rebuild : bool;     (* If [true], then a rebuild was requested. *)
}

val record :
  op:string ->
  key:string ->
  value:string ->
  job_id:string ->
  ready:Unix.tm ->
  running:Unix.tm option ->
  finished:Unix.tm ->
  build:int64 ->
  string Current.or_error ->
  unit
(** [record ~op ~key ~value ~job_id ~ready ~running ~finished ~build outcome] records that [key] is now set to [value],
    producing [outcome]. This replaces any previous entry.
    @param ready When the job was ready to start (i.e. enqueued).
    @param running When the job started running.
    @param finished When the job stopped running (i.e. now). *)

val init : unit -> unit

val lookup : op:string -> string -> entry option
(** [lookup ~op key] returns the most recently stored result for [op] and [key], if any. *)

val drop_all : string -> unit
(** [drop_all op] drops all cached entries for [op]. *)

val invalidate : op:string -> string -> unit
(** [invalidate ~op key] removes any existing entry for [op, key]. *)

val query : ?op:string -> ?ok:bool -> ?rebuild:bool ->unit -> entry list
(** Search the database for matching records. *)

val lookup_job_id : string -> (string * string) option
(** [lookup_job_id x] is the (op, key) of job [x], if known. *)

val history : limit:int -> op:string -> string -> entry list
