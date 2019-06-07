module type INPUT = sig
  type t
  (** An input that was used while evaluating a term.
      If the input changes, the term should be re-evaluated. *)
end

module type ANALYSIS = sig
  type t
  (** Information about the dependency graph of a term.
      This is useful to display the term's state as a diagram. *)

  val pp : t Fmt.t
  (** [pp] formats a [t] as a simple string. *)

  val pp_dot : t Fmt.t
  (** [pp_dot] formats a [t] as a graphviz dot graph. *)
end

module type TERM = sig
  type input
  (** See [INPUT]. *)

  type +'a t
  (** An ['a t] is a term that produces a value of type ['a]. *)

  val pending : unit -> 'a t
  (** [pending ()] is a term that never produces a result. *)

  val return : 'a -> 'a t
  (** [return x] is a term that immediately succeeds with [x]. *)

  val fail : string -> 'a t
  (** [fail m] is a term that immediately fails with message [m]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f x] is a term that runs [x] and then transforms the result using [f]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] is the pair containing the results of evaluating [a] and [b]
      (in parallel). *)

  val bind : ?name:string -> ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f x] is a term that first runs [x] to get [y] and then behaves as
      the term [f y]. Static analysis cannot look inside the [f] function until
      [x] is ready, so using [bind] makes static analysis less useful. You can
      use the [name] argument to name the box, at least. *)

  val list_map : ('a t -> 'b t) -> 'a list t -> 'b list t
  (** [list_map f xs] adds [f] to the end of each input term
      and collects all the results into a single list. *)

  val list_iter : ('a t -> unit t) -> 'a list t -> unit t
  (** Like [list_map] but for the simpler case when the result is unit. *)

  val all : unit t list -> unit t
  (** [all xs] is a term that succeeds if every term in [xs] succeeds. *)

  val gate : on:unit t -> 'a t -> 'a t
  (** [gate ~on:ctrl x] is the same as [x], once [ctrl] succeeds. *)

  val track : input -> 'a t -> 'a t
  (** [track i t] is like [t], but records that [t] depends on [i].
      This is needed to implement the primitive building-blocks that make up
      pipelines. *)

  module Syntax : sig
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    (** Syntax for [map]. Use this to process the result of a term without
        using any special effects. *)

    val (and+) : 'a t -> 'b t -> ('a * 'b) t
    (** Syntax for [pair]. Use this to depend on multiple terms. *)

    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
    (** Monadic [bind]. Use this if the next part of your pipeline can only
        be determined at runtime by looking at the concrete value. Static
        analysis cannot predict what this will do until the input is ready. *)

    val (let**) : 'a t -> ('a -> 'b t) -> string -> 'b t
    (** Like [let*], but allows you to name the operation.
        e.g. ["my-op" |> let** x = fetch uri in ...] *)

    val (and*) : 'a t -> 'b t -> ('a * 'b) t
    (** Syntax for [pair]. Use this to depend on multiple terms.
        Note: this is the same as [and+]. *)
  end
end

module type EXECUTOR = sig
  type 'a term
  (** See [TERM]. *)

  type input
  (** See [INPUT]. *)

  type analysis
  (** See [ANALYSIS]. *)

  type 'a output = ('a, [`Pending | `Msg of string]) result

  val pp_output : 'a Fmt.t -> 'a output Fmt.t

  val run : 'a term -> analysis * 'a output * input list
  (** [run t] evaluates term [t], returning whatever information about the
      term could be determined statically given the input, the current
      output, and the set of inputs that were used during the evaluation.

      If any of the inputs change, you should call [run] again to get the new
      results.

      To perform a pure static analysis of a pipeline function [f] before the
      input is known, use [run (f pending)]. *)
end
