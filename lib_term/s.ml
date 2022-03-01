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

module type ORDERED = sig
  include Map.OrderedType
  val pp : t Fmt.t
end

module type ANALYSIS = sig
  type 'a term
  (** See [TERM]. *)

  type metadata
  (** Extra data provided by primitives but that isn't part of the value that is
      passed on to other nodes. For example, a job ID. *)

  val metadata : 'a term -> metadata option term
  (** [metadata t] is the metadata of [t], if any.
      Raises an exception if [t] is not a primitive (or a map of one). *)

  val pp : _ term Fmt.t
  (** [pp] formats a [t] as a simple string. *)

  val pp_dot :
    env:(string * string) list ->
    collapse_link:(k:string -> v:string -> string option) ->
    job_info:(metadata -> Output.active option * string option) ->
    _ term Fmt.t
  (** [pp_dot ~env ~collapse_link ~job_info] formats a [t] as a graphviz dot graph.
      @param env A list of key-value pairs from the URL to control rendering.
      @param collapse_link Should return a link to the same page with "?k=v"
                           added to the environment.
      @param job_info Get update statuses and URLs for links to jobs.
                      The update status is used if the job is rebuilding while
                      showing the output of a previous run. The box is
                      displayed using a gradient from the update status colour
                      to the current output colour. *)

  (** {2 Stats} *)

  val stat : _ term -> stats
  (** [stat t] count how many stages are in each state.
      This can be slow for large pipelines. Consider using {!quick_stat} instead. *)

  val quick_stat : unit -> stats
  (** [quick_stat ()] returns the current values of the counters. This is O(1).
      It only counts some operations (binds, primitives and of_output). *)

end

module type TERM = sig
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

  val list_map : (module ORDERED with type t = 'a) -> ?collapse_key:string -> ?label:string -> ('a t -> 'b t) -> 'a list t -> 'b list t
  (** [list_map (module T) f xs] adds [f] to the end of each input term
      and collects all the results into a single list.
      @param T Used to display labels for each item, and to avoid recreating pipelines
               unnecessarily.
      @param collapse_key If given, each element is wrapped with [collapse]. 
      @param label Label the list in the diagrams. *)

  val list_iter : (module ORDERED with type t = 'a) -> ?collapse_key:string -> ?label:string -> ('a t -> unit t) -> 'a list t -> unit t
  (** Like [list_map] but for the simpler case when the result is unit. 
      @param label Label the list in the diagrams.*)

  val list_seq : 'a t list -> 'a list t
  (** [list_seq x] evaluates to a list containing the results of evaluating
      each element in [x], once all elements of [x] have successfully completed. *)

  val option_map : ?label:string -> ('a t -> 'b t) -> 'a option t -> 'b option t
  (** [option_map f x] is a term that evaluates to [Some (f y)] if [x]
      evaluates to [Some y], or to [None] otherwise. 
      @param label Label the optional in the diagrams. *)

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
  (** [gate ~on:ctrl x] is the same as [x], once [ctrl] succeeds.

      Note: [gate] does {i not} delay [x]; it only delays whatever you put after the gate.
      e.g.
      {[
      let binary = build src in
      let tests_ok = test binary in
      binary |> gate ~on:tests_ok |> deploy
      ]}
  *)

  (** {2 Diagram control} *)

  val collapse : key:string -> value:string -> input:_ t -> 'a t -> 'a t
  (** [collapse ~key ~value ~input t] is a term that behaves just like [t], but
      when shown in a diagram it can be expanded or collapsed. When collapsed,
      it is shown as "input -> [+]" and the user can expand it to show [t]
      instead. The idea is that [input] is a dependency of [t] and the "+"
      represents everything in [t] after that. [key] and [value] are used
      as the parameters (e.g. in a URL) to control whether this is expanded or
      not. For example
      [collapse ~key:"repo" ~value:"mirage/mirage-www" ~input:repo (process repo)]
      Note: [list_map ~collapse_key] provides an easy way to use this. *)

  val collapse_list : key:string -> value:string -> input:_ t -> 'a t list -> 'a t list * unit t
  (** [collapse_list ~key ~value ~input t] is a term that behaves just like [t] list, but
      when shown in a diagram it can be expanded or collapsed. *)

  val with_context : _ t -> (unit -> 'a t) -> 'a t
  (** [with_context ctx f] is the term [f ()], where [f] is evaluated in
      context [ctx]. This means that [ctx] will be treated as an input to all
      terms created by [f] in the diagrams. *)

  (** {2 Monadic operations} *)

  (** {b N.B.} these operations create terms that cannot be statically
      analysed until after they are executed. *)

  val bind : ?info:description -> ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f x] is a term that first runs [x] to get [y] and then behaves as
      the term [f y]. Static analysis cannot look inside the [f] function until
      [x] is ready, so using [bind] makes static analysis less useful. You can
      use the [info] argument to provide some information here. *)

  (** {2 Primitives} *)

  type metadata
  (** See [ANALYSIS]. *)

  type 'a primitive

  val primitive : info:description -> ('a -> 'b primitive) -> 'a t -> 'b t
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

    val (let>) : 'a t -> ('a -> 'b primitive) -> description -> 'b t
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
