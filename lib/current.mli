type 'a or_error = ('a, [`Msg of string]) result

module Level = Level

module Config : sig
  type t

  val v : ?auto_release:Duration.t -> ?confirm:Level.t -> unit -> t
  (** A new configuration.
      @param auto_release Remove confirmation requirement this period (unless changed manually first).
      @param confirm Confirm before performing operations at or above this level. *)

  val set_confirm : t -> Level.t option -> unit
  (** Change the [confirm] setting. Existing jobs waiting for confirmation
      will now start if permitted by the new configuration. *)

  val get_confirm : t -> Level.t option

  val cmdliner : t Cmdliner.Term.t

  val now : t option ref
  (** [None] initially, then set to the configuration and never changes. *)
end

type job_id = string

class type actions = object
  method pp : Format.formatter -> unit
  (** Format a message for the user explaining what is being waited on. *)

  method rebuild : (unit -> job_id) option
  (** A function to call if the user explicitly requests the operation be done again,
      or [None] if it is not something that can be repeated. Returns the new job ID. *)

  method release : unit
  (** Called to release the caller's reference to the watch (reduce the
      ref-count by 1). Some inputs may cancel a build if the ref-count
      reaches zero. *)
end

module Input : sig
  type 'a t
  (** An input that produces an ['a term]. *)

  val const : 'a -> 'a t
  (** [const x] is an input that always evaluates to [x] and never needs to be updated. *)

  val register_actions : ?job_id:job_id -> actions -> unit
  (** [register_actions ~job_id actions] is used to register handlers for
      cancelling and rebuilding jobs.
      Once evaluation is complete, [actions#release] will be called.
      If the ref-count drops to zero then you can then cancel the job.
      @param job_id An ID that can be used to refer to this job later (to request a rebuild, etc).
      @param actions Ways to interact with this input. *)

  val of_fn : (unit -> 'a Current_term.Output.t * job_id option) -> 'a t
  (** [of_fn f] is an input that calls [f ()] when it is evaluated.
      [f] can call [register] to attach actions to a job. *)

  val map_result : ('a Current_term.Output.t -> 'b Current_term.Output.t) -> 'a t -> 'b t
  (** [map_result fn t] transforms the result of [t] with [fn]. The metadata remains the same.
      If [fn] raises an exception, this is converted to [Error]. *)
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
    jobs : actions Job_map.t;        (** The jobs currently being used (whether running or finished). *)
  }

  val create :
    ?config:Config.t ->
    ?trace:(next:unit Lwt.t -> results -> unit Lwt.t) ->
    (unit -> unit term) ->
    t
  (** [create pipeline] is a new engine running [pipeline].
      The engine will evaluate [t]'s pipeline immediately, and again whenever
      one of its inputs changes. *)

  val update : unit -> unit
  (** Trigger a reevaluation of the pipeline.
      Inputs should call this whenever they might now produce a different result
      (e.g. an active input becomes finished). *)

  val state : t -> results
  (** The most recent results from evaluating the pipeline. *)

  val jobs : results -> actions Job_map.t

  val thread : t -> 'a Lwt.t
  (** [thread t] is the engine's thread.
      Use this to monitor the engine (in case it crashes). *)

  val actions : metadata -> actions

  val job_id : metadata -> job_id option

  val config : t -> Config.t

  val pp_metadata : metadata Fmt.t

  val update_metrics : results -> unit
  (** [update_metrics results] reports how many pipeline stages are in each state via Prometheus.
      Call this on each metrics collection if you have exactly one pipeline. The default web
      UI does this automatically. *)

  module Step : sig
    type t
    (** A unique ID representing the current iteration.
        This is used by the cache to warn about attempts to set the same output
        to two different values at the same time. *)

    val equal : t -> t -> bool

    val now : unit -> t
  end
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
  val digest : t -> string
  val marshal : t -> string
  val unmarshal : string -> t
end

module Switch : sig
  (** Like [Lwt_switch], but the cleanup functions are called in sequence, not
      in parallel. *)

  type t
  (** A switch limits the lifetime of an operation.
      Cleanup operations can be registered against the switch and will
      be called (in reverse order) when the switch is turned off. *)

  val create : label:string -> unit -> t
  (** [create ~label ()] is a fresh switch, initially on.
      @param label If the switch is GC'd while on, this is logged in the error message. *)

  val create_off : unit -> t
  (** [create_off ()] is a fresh switch, initially (and always) off. *)

  val add_hook_or_exec : t -> (unit -> unit Lwt.t) -> unit Lwt.t
  (** [add_hook_or_exec switch fn] pushes [fn] on to the stack of functions to call
      when [t] is turned off. If [t] is already off, calls [fn] immediately.
      If [t] is in the process of being turned off, waits for that to complete
      and then runs [fn]. *)

  val turn_off : t -> unit Lwt.t
  (** [turn_off t] marks the switch as being turned off, then pops and
      calls clean-up functions in order. When the last one finishes, the switch
      is marked as off and cannot be used again. If the switch is already off,
      this does nothing. If the switch is already being turned off, it just
      waits for that to complete. *)

  val is_on : t -> bool
  (** [is_on t] is [true] if [turn_off t] hasn't yet been called. *)

  val pp : t Fmt.t
  (** Prints the state of the switch (for debugging). *)
end

module Pool : sig
  type t
  (** A pool of resources, to control how many jobs can use a resource at a time. *)

  val create : label:string -> int -> t
  (** [create ~label n] is a pool with [n] resources.
      @param label Used for metric reporting and logging. *)
end

module Job : sig
  type t

  val create : switch:Switch.t -> label:string -> config:Config.t -> unit -> t
  (** [create ~switch ~label ~config ()] is a new job.
      @param switch Turning this off will cancel the job.
      @param label A label to use in the job's filename (for debugging). *)

  val start : ?timeout:Duration.t -> ?pool:Pool.t -> level:Level.t -> t -> unit Lwt.t
  (** [start t ~level] marks [t] as running. This can only be called once per job.
      If confirmation has been configured for [level], then this will wait for confirmation first.
      @param timeout If given, the job will be cancelled automatically after this period of time.
      @param pool If given, the job cannot start until a pool resource is available.
                  The resource is freed when the job finishes. *)

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

  val pp_id : job_id Fmt.t

  val is_running : t -> bool
  (** [is_running t] is true if the log file is still open. *)

  val wait_for_log_data : t -> unit Lwt.t
  (** [wait_for_log_data t] is a promise that resolves the next time log data
      is written or the log is closed. *)

  val lookup_running : job_id -> t option
  (** If [lookup_running job_id] is the job [j] with id [job_id], if [is_running j]. *)

  val jobs : unit -> t Job_map.t
  (** [jobs ()] is the set of active jobs, whether they are currently used in a pipeline or not.
      This is any job which is running or ready to run (i.e. every job which hasn't closed its log file). *)

  val approve_early_start : t -> unit
  (** [approve_early_start t] marks the job as approved to start even if the
      global confirmation threshold would otherwise prevent it. Calling this
      more than once has no effect. *)

  val is_waiting_for_confirmation : t -> bool
  (** Indicates whether the job would benefit from [approve_early_start] being called. *)

  val on_cancel : t -> (string -> unit Lwt.t) -> unit Lwt.t
  (** [on_cancel t fn] calls [fn reason] if the job is cancelled.
      If the job has already been cancelled, [fn] is called immediately.
      If a job finishes without being cancelled, the cancel hooks are run at the end anyway. *)

  val with_handler : t -> on_cancel:(string -> unit Lwt.t) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_handler t ~on_cancel fn] is like [fn ()], but if the job is cancelled while [fn] is running
      then [on_cancel reason] is called. Even if cancelled, [fn] is still run to completion.
      If the job has already been cancelled then [fn reason] is called immediately (but [fn] still runs). *)

  val cancel : t -> string -> unit
  (** [cancel msg] requests that [t] be cancelled. This runs all registered cancel hooks and marks the job as cancelled.
      If the job is already cancelled, this has no effect. *)

  val cancelled_state : t -> unit or_error
  (** [cancelled_state t] is [Ok ()] if the job hasn't been cancelled, or [Error (`Msg reason)] if it has.
      This should not be used after the switch has been turned off. *)

  (**/**)

  (* For unit tests we need our own test clock: *)

  val timestamp : (unit -> float) ref
  val sleep : (float -> unit Lwt.t) ref
end

module Process : sig
  val exec :
    ?stdin:string ->
    ?pp_error_command:(Format.formatter -> unit) ->
    cancellable:bool ->
    job:Job.t -> Lwt_process.command ->
    unit or_error Lwt.t
  (** [exec ~job cmd] uses [Lwt_process] to run [cmd], with output to [job]'s log.
      @param cancellable Should the process be terminated if the job is cancelled?
      @param stdin Data to write to stdin before closing it.
      @param pp_error_command Format the command for an error message.
        The default is to print "Command $cmd". *)

  val check_output :
    ?cwd:Fpath.t -> ?stdin:string ->
    ?pp_error_command:(Format.formatter -> unit) ->
    cancellable:bool ->
    job:Job.t -> Lwt_process.command ->
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

  val dump_row : Sqlite3.Data.t list Fmt.t
  (** Useful for debugging. *)
end

module Log_matcher : sig
  type rule = {
    pattern : string;
    report : string;
    score : int;
  }

  val analyse_job : Job.t -> string option
  (** [analyse_job j] scans the logs for [j] looking for known patterns.
      If it finds any, it returns the report string for the best one, and
      also logs information about all found patterns. *)

  val add_rule : rule -> unit
  (** Add a rule that matches log text against the PCRE [rule.pattern].
      If it matches, the error will be [rule.report]. In [report], "\1" is
      replaced by the first match group, etc. If multiple rules match, the one
      with the highest scrore is used. If two rules with the same score match,
      the first is used. If a rule already exists with the same pattern, it
      will be replaced. *)

  val remove_rule : string -> (unit, [> `Rule_not_found ]) result
  (** [remove_rule pattern] removes a rule previously added by [add_rule]. *)

  val list_rules : unit -> rule list
  (** Get the current set of rules. *)

  (**/**)

  (* For unit-tests: *)

  val drop_all : unit -> unit
  val analyse_string : ?job:Job.t -> string -> string option
end
