module Input : sig
  class type watch = object
    method pp : Format.formatter -> unit
    method changed : unit Lwt.t
    method release : unit
  end

  type 'a t

  val of_fn : (unit -> 'a Current_term.Output.t * watch) -> 'a t
  (** [of_fn f] is an input that calls [f ()] when it is evalutated.
      When [f] is called, the caller gets a ref-count on [watch] and will
      call [release] exactly once when the watch is no longer needed.

      Note: the engine calls [f] in an evaluation before calling [release]
      on the previous watch, so if the ref-count drops to zero then you can
      cancel the job. *)

  val pp_watch : watch Fmt.t
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
