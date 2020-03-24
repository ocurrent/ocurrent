type 'a or_error = ('a, [`Msg of string]) result

type stats = {
  ok : int;
  ready : int;
  running : int;
  failed : int;
  blocked : int;
}
(** Counters showing how many pipeline stages are in each state. *)

module type T = sig
  type t
  val equal : t -> t -> bool
  val pp : t Fmt.t
end

module type INPUT = sig
  type 'a t
  (** An input that was used while evaluating a term.
      If the input changes, the term should be re-evaluated. *)

  type job_id

  val get : 'a t -> ('a Output.t * job_id option) Current_incr.t
end

module type ANALYSIS = sig
  type 'a term
  (** See [TERM]. *)

  type job_id

  val job_id : 'a term -> job_id option term
  (** [job_id t] is the job ID of [t], if any.
      Raises an exception if [t] is not a primitive (or a map of one). *)

  val pp : _ term Fmt.t
  (** [pp] formats a [t] as a simple string. *)

  val pp_dot : url:(job_id -> string option) -> _ term Fmt.t
  (** [pp_dot ~url] formats a [t] as a graphviz dot graph.
      @param url Generates a URL from an ID. *)

  val stats : _ term -> stats
  (** [stats t] count how many stages are in each state. *)
end

module type TERM = sig
  type 'a input
  (** See [INPUT]. *)

  type 'a t
  (** An ['a t] is a term that produces a value of type ['a]. *)

  type description
  (** Information about operations hidden behind a bind. *)

  val active : Output.active -> 'a t
  (** [active x] is a term indicating that the result is not determined yet. *)

  val return : ?label:string -> 'a -> 'a t
  (** [return x] is a term that immediately succeeds with [x].
      @param label Label the constant in the diagrams. *)

  val fail : string -> 'a t
  (** [fail m] is a term that immediately fails with message [m]. *)

  val state : ?hidden:bool -> 'a t -> ('a, [`Active of Output.active | `Msg of string]) result t
  (** [state t] always immediately returns a successful result giving the current state of [t].
      @param hidden If [true], don't show a separate node for this on the diagrams. *)

  val catch : ?hidden:bool -> 'a t -> 'a or_error t
  (** [catch t] successfully returns [Ok x] if [t] evaluates successfully to [x],
      or successfully returns [Error e] if [t] fails with error [e].
      If [t] is active then [catch t] will be active too.
      @param hidden If [true], don't show a separate node for this on the diagrams. *)

  val ignore_value : 'a t -> unit t
  (** [ignore_value x] is [map ignore x]. *)

  val of_output : 'a Output.t -> 'a t
  (** [of_output x] is a returned, failed or active term. *)

  (** {1 Sequencing terms} *)

  (** {2 Applicative operations} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f x] is a term that runs [x] and then transforms the result using [f]. *)

  val map_error : (string -> string) -> 'a t -> 'a t
  (** [map_error f x] is a term that runs [x] and then transforms the error string (if any) using [f]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] is the pair containing the results of evaluating [a] and [b]
      (in parallel). *)

  val list_map : pp:'a Fmt.t -> ('a t -> 'b t) -> 'a list t -> 'b list t
  (** [list_map ~pp f xs] adds [f] to the end of each input term
      and collects all the results into a single list.
      @param pp Label the instances. *)

  val list_iter : pp:'a Fmt.t -> ('a t -> unit t) -> 'a list t -> unit t
  (** Like [list_map] but for the simpler case when the result is unit. *)

  val list_seq : 'a t list -> 'a list t
  (** [list_seq x] evaluates to a list containing the results of evaluating
      each element in [x], once all elements of [x] have successfully completed. *)

  val option_map : ('a t -> 'b t) -> 'a option t -> 'b option t
  (** [option_map f x] is a term that evaluates to [Some (f y)] if [x]
      evaluates to [Some y], or to [None] otherwise. *)

  val option_seq : 'a t option -> 'a option t
  (** [option_seq None] is [Current.return None] and
      [option_seq (Some x)] is [Current.map some x].
      This is useful for handling optional arguments that are currents. *)

  val all : unit t list -> unit t
  (** [all xs] is a term that succeeds if every term in [xs] succeeds. *)

  val all_labelled : (string * unit t) list -> unit t
  (** [all xs] is a term that succeeds if every term in [xs] succeeds.
      The labels are used if some terms fail, to indicate which ones
      are failing. *)

  val gate : on:unit t -> 'a t -> 'a t
  (** [gate ~on:ctrl x] is the same as [x], once [ctrl] succeeds. *)

  (** {2 Monadic operations} *)

  (** {b N.B.} these operations create terms that cannot be statically
      analysed until after they are executed. *)

  val bind : ?info:description -> ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f x] is a term that first runs [x] to get [y] and then behaves as
      the term [f y]. Static analysis cannot look inside the [f] function until
      [x] is ready, so using [bind] makes static analysis less useful. You can
      use the [info] argument to provide some information here. *)

  val primitive : info:description -> ('a -> 'b input) -> 'a t -> 'b t
  (** [primitive ~info f x] is a term that evaluates [f] on each new value of [x].
      This is used to provide the primitive operations, which can then be
      combined using the other combinators in this module.
      [info] is used to label the operation in the diagram. *)

  val component : ('a, Format.formatter, unit, description) format4 -> 'a
  (** [component name] is used to annotate binds, so that the system can show a
      name for the operations hidden inside the bind's function. [name] is used
      as the label for the bind in the generated dot diagrams.
      For convenience, [name] can also be a format string. *)

  module Syntax : sig
    (** {1 Applicative syntax} *)

    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    (** Syntax for {!map}. Use this to process the result of a term without
        using any special effects. *)

    val (and+) : 'a t -> 'b t -> ('a * 'b) t
    (** Syntax for {!pair}. Use this to depend on multiple terms. *)

    (** {1 Monadic syntax } *)

    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
    (** Monadic {!bind}. Use this if the next part of your pipeline can only
        be determined at runtime by looking at the concrete value. Static
        analysis cannot predict what this will do until the input is ready. *)

    val (let>) : 'a t -> ('a -> 'b input) -> description -> 'b t
    (** [let>] is used to define a component. e.g.:
        {[
          component "my-op" |>
            let> x = fetch uri in
            ...
        ]} *)

    val (let**) : 'a t -> ('a -> 'b t) -> description -> 'b t
    (** Like {!let*}, but allows you to name the operation. e.g.:
        {[
          component "my-op" |>
            let** x = fetch uri in
            ...
        ]} *)

    val (and*) : 'a t -> 'b t -> ('a * 'b) t
    (** Syntax for {!pair}. Use this to depend on multiple terms.
        Note: this is the same as {!and+}. *)

    val (and>) : 'a t -> 'b t -> ('a * 'b) t
    (** Syntax for {!pair}. Use this to depend on multiple terms.
        Note: this is the same as {!and+}. *)
  end
end

module type EXECUTOR = sig
  type 'a term
  (** See [TERM]. *)

  val run : 'a term -> 'a Output.t Current_incr.t
  (** [run t] is the output value of [t] (i.e. without the static analysis part). *)
end
