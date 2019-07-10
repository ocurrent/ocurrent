(** Cache build results in memory (or, TODO, on disk).
    A cache maps keys to values. Looking up a key that isn't known starts a new
    build to create it. *)

module Job : sig
  type t

  val write : t -> string -> unit
  (** [write t data] appends [data] to the log. *)

  val log : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log t fmt] appends a formatted message to the log, with a newline added at the end. *)

  val fd : t -> Unix.file_descr
end

module S = S

module Schedule : sig
  type t

  val v : ?valid_for:Duration.t -> unit -> t
  (** Create a new configuration.
      @param valid_for Consider a cached entry invalid after this long
   *)
end

module Make (B : S.BUILDER with type job := Job.t) : sig
  val get : ?schedule:Schedule.t -> B.t -> B.Key.t -> B.Value.t Current.t
  (** [get b k] is a term for the result of building [k]. *)

  val invalidate : B.Key.t -> unit
  (** [invalidate key] removes key from the cache. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Output (P : S.PUBLISHER with type job := Job.t) : sig
  val set : P.t -> P.Key.t -> P.Value.t -> unit Current.t
  (** [set p k v] is a term for the result of setting [k] to [v]. *)

  val reset : unit -> unit
  (** [reset ()] clears the cache. Useful for unit-tests. *)
end

module Process : sig
  val exec :
    ?switch:Lwt_switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
    unit Current.or_error Lwt.t
  (** [exec ~job cmd] uses [Lwt_process] to run [cmd], with output to [job]'s log.
      @param switch If this is turned off, the process is terminated.
      @param stdin Data to write to stdin before closing it. *)

  val check_output :
    ?switch:Lwt_switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
    string Current.or_error Lwt.t
  (** Like [exec], but return the child's stdout as a string rather than writing it to the log. *)

  val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_tmpdir fn] creates a temporary directory, runs [fn tmpdir], and then deletes the directory
      (recursively).
      @param prefix Allows giving the directory a more meaningful name (for debugging). *)
end

(**/**)

(* For unit tests we need our own test clock: *)

val timestamp : (unit -> float) ref
val sleep : (float -> unit Lwt.t) ref
