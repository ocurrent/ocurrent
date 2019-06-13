type 'a or_error = ('a, [`Msg of string]) result

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

include Current_term.S.TERM with type 'a input := 'a Input.t

type 'a term = 'a t
(** An alias of [t] to make it easy to refer to later in this file. *)

module Analysis : Current_term.S.ANALYSIS with type 'a term := 'a t

module Engine : sig
  val run :
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
