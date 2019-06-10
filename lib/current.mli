module Input : sig
  class type t = object
    method pp : Format.formatter -> unit
    method changed : unit Lwt.t
  end

  val pp : t Fmt.t
end

include Current_term.S.TERM with type input := Input.t

type 'a term = 'a t
(** An alias of [t] to make it easy to refer to later in this file. *)

module Analysis : Current_term.S.ANALYSIS with type 'a term := 'a t

module Engine : sig
  val run :
    ?trace:(unit Current_term.Output.t -> Input.t list -> unit) ->
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
