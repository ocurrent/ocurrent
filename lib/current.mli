type 'a or_error = ('a, [`Msg of string]) result

module Level = Level

module Input : sig
  class type watch = object
    method pp : Format.formatter -> unit
    (** Format a message for the user explaining what is being waited on. *)

    method changed : unit Lwt.t
    (** A Lwt promise that resolves when the input has changed (and so terms
        using it should be recalculated). *)

    method cancel : (unit -> unit) option
    (** A function to call if the user explicitly requests the operation be cancelled,
        or [None] if it is not something that can be cancelled. *)

    method release : unit
    (** Called to release the caller's reference to the watch (reduce the
        ref-count by 1). Some inputs may cancel a build if the ref-count
        reaches zero. *)
  end

  type 'a t
  (** An input that produces an ['a term]. *)

  val of_fn : (unit -> 'a Current_term.Output.t * watch list) -> 'a t
  (** [of_fn f] is an input that calls [f ()] when it is evalutated.
      When [f] is called, the caller gets a ref-count on the watches and will
      call [release] exactly once when each watch is no longer needed.

      Note: the engine calls [f] in an evaluation before calling [release]
      on the previous watches, so if the ref-count drops to zero then you can
      cancel the job. *)

  val pp_watch : watch Fmt.t
  (** [pp_watch f w] is [w#pp f]. *)
end

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

module Analysis : Current_term.S.ANALYSIS with type 'a term := 'a t

module Config : sig
  type t

  val v : ?confirm:Level.t -> unit -> t
  (** A new configuration.
      @param confirm : confirm before performing operations at or above this level. *)

  val set_confirm : t -> Level.t option -> unit
  (** Change the [confirm] setting. Existing jobs waiting for confirmation
      will now start if permitted by the new configuration. *)

  val cmdliner : t Cmdliner.Term.t
end

val confirmed : Level.t -> unit Lwt.t term
(** [confirmed l] evaluates to a promise that resolves once we are ready to run
    an action at level [l] or higher. *)

module Engine : sig
  val run :
    ?config:Config.t ->
    ?trace:(unit Current_term.Output.t -> Input.watch list -> unit) ->
    (unit -> unit t) ->
    'a Lwt.t
  (** [run f] evaluates [f ()] immediately, and again whenever one of its
      input changes. It doesn't return. *)
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
end

module Unit : sig
  (** Missing from the OCaml standard library. *)

  type t = unit

  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end
