type 'a or_error = ('a, [`Msg of string]) result

module Level = Level

module Config : sig
  type t

  val v : ?confirm:Level.t -> unit -> t
  (** A new configuration.
      @param confirm : confirm before performing operations at or above this level. *)

  val set_confirm : t -> Level.t option -> unit
  (** Change the [confirm] setting. Existing jobs waiting for confirmation
      will now start if permitted by the new configuration. *)

  val get_confirm : t -> Level.t option

  val cmdliner : t Cmdliner.Term.t
end

type job_id = string

class type actions = object
  method pp : Format.formatter -> unit
  (** Format a message for the user explaining what is being waited on. *)

  method cancel : (unit -> unit) option
  (** A function to call if the user explicitly requests the operation be cancelled,
      or [None] if it is not something that can be cancelled. *)

  method rebuild : (unit -> unit) option
  (** A function to call if the user explicitly requests the operation be done again,
      or [None] if it is not something that can be repeated. *)

  method release : unit
  (** Called to release the caller's reference to the watch (reduce the
      ref-count by 1). Some inputs may cancel a build if the ref-count
      reaches zero. *)
end

module Step : sig
  type t
  type id

  val id : t -> id
  (** [id t] is a unique value for this evaluation step.
      This can be useful to detect if e.g. the same output has been set to two different values in one step. *)

  val config : t -> Config.t
end

module Input : sig
  type 'a t
  (** An input that produces an ['a term]. *)

  type metadata
  (** Information about a value of the input at some point. *)

  val const : 'a -> 'a t
  (** [const x] is an input that always evaluates to [x] and never needs to be updated. *)

  val metadata :
    ?job_id:job_id ->
    ?changed:unit Lwt.t ->
    actions -> metadata
  (** [metadata actions] is used to provide metadata about a value.
      @param job_id An ID that can be used to refer to this job later (to request a rebuild, etc).
      @param changed A Lwt promise that resolves when the input has changed (and so terms
                     using it should be recalculated).
      @param actions Ways to interact with this input. *)

  val of_fn : (Step.t -> 'a Current_term.Output.t * metadata) -> 'a t
  (** [of_fn f] is an input that calls [f config] when it is evaluated.
      When [f] is called, the caller gets a ref-count on the watches and will
      call [release] exactly once when each watch is no longer needed.

      Note: the engine calls [f] in an evaluation before calling [release]
      on the previous watches, so if the ref-count drops to zero then you can
      cancel the job. *)
end

module Job_map : Map.S with type key = job_id

val monitor :
  read:(unit -> 'a or_error Lwt.t) ->
  watch:((unit -> unit) -> (unit -> unit Lwt.t) Lwt.t) ->
  pp:(Format.formatter -> unit) ->
  'a Input.t
(** [monitor ~read ~watch ~pp] is an input that uses [read] to read the current
    value of some external resource and [watch] to watch for changes. When the
    input is needed, it first calls [watch refresh] to start watching the
    resource. When this completes, it uses [read ()] to read the current value.
    Whenever the watch thread calls [refresh] it marks the value as being
    out-of-date and will call [read] to get a new value. When the input is no
    longer required, it will call the shutdown function returned by [watch] to
    stop watching the resource. If it is needed later, it will run [watch] to
    start watching it again. This function takes care to perform only one user
    action (installing the watch, reading the value, or turning off the watch)
    at a time. For example, if [refresh] is called while already reading a
    value then it will wait for the current read to complete and then perform a
    second one. *)

include Current_term.S.TERM with type 'a input := 'a Input.t

type 'a term = 'a t
(** An alias of [t] to make it easy to refer to later in this file. *)

module Analysis : Current_term.S.ANALYSIS with
  type 'a term := 'a t and
  type job_id := job_id

module Engine : sig
  type t

  type metadata

  type results = {
    value : unit Current_term.Output.t;
    analysis : Analysis.t;
    watches : metadata list;
    jobs : actions Job_map.t;        (** The jobs currently being used (whether running or finished). *)
  }

  val create :
    ?config:Config.t ->
    ?trace:(results -> unit Lwt.t) ->
    (unit -> unit term) ->
    t
  (** [create pipeline] is a new engine running [pipeline].
      The engine will evaluate [t]'s pipeline immediately, and again whenever
      one of its inputs changes. *)

  val state : t -> results
  (** The most recent results from evaluating the pipeline. *)

  val thread : t -> 'a Lwt.t
  (** [thread t] is the engine's thread.
      Use this to monitor the engine (in case it crashes). *)

  val actions : metadata -> actions

  val job_id : metadata -> job_id option

  val config : t -> Config.t

  val is_stale : metadata -> bool
  (** [is_stale m] is [true] if this job has signalled that
      it should be re-evaluated. Provided for unit-tests. *)

  val pp_metadata : metadata Fmt.t
end

module Var (T : Current_term.S.T) : sig
  type t
  (** A variable with a current value of type [T.t Current_term.Output.t]. *)

  val get : t -> T.t term

  val create : name:string -> T.t Current_term.Output.t -> t
  val set : t -> T.t Current_term.Output.t -> unit
  val update : t -> (T.t Current_term.Output.t -> T.t Current_term.Output.t) -> unit
end

val state_dir : string -> Fpath.t
(** [state_dir name] is a directory under which state (build results, logs) can be stored.
    [name] identifies the sub-component of OCurrent, each of which gets its own subdirectory. *)

module String : sig
  type t = string
  val digest : t -> string
  val pp : t Fmt.t
  val equal : t -> t -> bool
  val marshal : t -> string
  val unmarshal : string -> t
end

module Unit : sig
  (** Missing from the OCaml standard library. *)

  type t = unit

  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val marshal : t -> string
  val unmarshal : string -> t
end

module Switch : sig
  (** Like [Lwt_switch], but the cleanup functions are called in sequence, not
      in parallel, and a reason for the shutdown may be given. *)

  type t
  (** A switch limits the lifetime of an operation.
      Cleanup operations can be registered against the switch and will
      be called (in reverse order) when the switch is turned off. *)

  val create : label:string -> unit -> t
  (** [create ~label ()] is a fresh switch, initially on.
      @param label If the switch is GC'd while on, this is logged in the error message. *)

  val create_off : unit or_error -> t
  (** [create_off reason] is a fresh switch, initially (and always) off. *)

  val add_hook_or_exec : t -> (unit or_error -> unit Lwt.t) -> unit Lwt.t
  (** [add_hook_or_exec switch fn] pushes [fn] on to the stack of functions to call
      when [t] is turned off. If [t] is already off, calls [fn] immediately.
      If [t] is in the process of being turned off, waits for that to complete
      and then runs [fn]. *)

  val add_hook_or_exec_opt : t option -> (unit or_error -> unit Lwt.t) -> unit Lwt.t
  (** [add_hook_or_exec_opt] is like [add_hook_or_exec], but does nothing if the switch
      is [None]. *)

  val turn_off : t -> unit or_error -> unit Lwt.t
  (** [turn_off t reason] marks the switch as being turned off, then pops and
      calls clean-up functions in order. When the last one finishes, the switch
      is marked as off and cannot be used again. [reason] is passed to the
      cleanup functions, which may be useful for logging. If the switch is
      already off, this does nothing. If the switch is already being turned
      off, it just waits for that to complete. *)

  val is_on : t -> bool
  (** [is_on t] is [true] if [turn_off t] hasn't yet been called. *)

  val add_timeout : t -> Duration.t -> unit
  (** [add_timeout t duration] adds a timeout that will wait for [duration] and
      then turn off the switch. *)

  val pp : t Fmt.t
  (** Prints the state of the switch (for debugging). *)
end

module Job : sig
  type t

  val create : switch:Switch.t -> label:string -> config:Config.t -> unit -> t
  (** [create ~switch ~label ~config ()] is a new job.
      @param switch Turning this off will cancel the job.
      @param label A label to use in the job's filename (for debugging). *)

  val start : ?timeout:Duration.t -> level:Level.t -> t -> unit Lwt.t
  (** [start t ~level] marks [t] as running. This can only be called once per job.
      If confirmation has been configured for [level], then this will wait for confirmation first.
      @param timeout If given, the job will be cancelled automatically after this period of time. *)

  val start_time : t -> float Lwt.t
  (** [start_time t] is the time when [start] was called, or an
      unresolved promise for it if [start] hasn't been called yet. *)

  val write : t -> string -> unit
  (** [write t data] appends [data] to the log. *)

  val log : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log t fmt] appends a formatted message to the log, with a newline added at the end. *)

  val id : t -> job_id
  (** [id t] is the unique identifier for this job. *)

  val log_path : job_id -> Fpath.t or_error
  (** [log_path id] is the path of the log for job [id], if valid. *)

  val fd : t -> Unix.file_descr

  val pp_id : job_id Fmt.t

  (**/**)

  (* For unit tests we need our own test clock: *)

  val timestamp : (unit -> float) ref
  val sleep : (float -> unit Lwt.t) ref
end

module Process : sig
  val exec :
    ?switch:Switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
    unit or_error Lwt.t
  (** [exec ~job cmd] uses [Lwt_process] to run [cmd], with output to [job]'s log.
      @param switch If this is turned off, the process is terminated.
      @param stdin Data to write to stdin before closing it. *)

  val check_output :
    ?switch:Switch.t -> ?cwd:Fpath.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
    string or_error Lwt.t
  (** Like [exec], but return the child's stdout as a string rather than writing it to the log. *)

  val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_tmpdir fn] creates a temporary directory, runs [fn tmpdir], and then deletes the directory
      (recursively).
      @param prefix Allows giving the directory a more meaningful name (for debugging). *)
end

module Db : sig
  type t = Sqlite3.db

  val v : t Lazy.t
  (** An sqlite database stored in [state_dir "db"]. *)

  val exec : Sqlite3.stmt -> Sqlite3.Data.t list -> unit
  (** [exec stmt values] executes [stmt values].
      Raises an exception on error. *)

  val query : Sqlite3.stmt -> Sqlite3.Data.t list -> Sqlite3.Data.t list list
  (** [query stmt values] executes the SQL query [stmt values] and returns the resulting rows. *)

  val query_one : Sqlite3.stmt -> Sqlite3.Data.t list -> Sqlite3.Data.t list
  (** [query_one stmt values] executes the SQL query [stmt values] and returns the single resulting row.
      Raises an exception if there are no results or multiple results. *)

  val query_some : Sqlite3.stmt -> Sqlite3.Data.t list -> Sqlite3.Data.t list option
  (** [query_some stmt values] executes the SQL query [stmt values] and returns the single resulting row,
      or [None] if there are no results.
      Raises an exception if there are multiple results. *)

  val exec_literal : t -> string -> unit
  (** [exec_literal t sql] executes [sql] on [t].
      Raises an exception on error. *)
end
