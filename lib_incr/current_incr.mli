(** A simple library for incremental computations.
    Based on "Adaptive Functional Programming"
    https://www.cs.cmu.edu/~guyb/papers/popl02.pdf

    The basic idea is:

    - You run a computation, which produces a result.
    - The library records information about the dependencies of the result.
    - You can then change one or more inputs and it will rerun just the
      parts of the computation that are needed to update the result.

    It is similar to Jane Street's "incremental" library, but much smaller and
    has no external dependencies.

    It is also similar to "react", but the results do not depend on the
    behaviour of the garbage collector. In particular, functions stop being
    called as soon as they are no longer needed, and cannot be called with
    "impossible" inputs. *)

(** {2 Changeable values} *)

type 'a t
(** An ['a t] holds a value of type ['a], which will update as necessary. *)

val const : 'a -> 'a t
(** [const x] is the constant value [x]. *)

(** {2 Changeable computations} *)

type 'a cc
(** An ['a cc] is a changable computation that can be run to get a value of type ['a].
    Internally, it is a function that takes a destination variable, reads zero or more
    other changable values, and then updates the destination. *)

val read : 'a t -> ('a -> 'b cc) -> 'b cc
(** [read x f] is a computation that depends on [x]. [f] will be called as necessary to
    ensure that the result stays up-to-date with changes in [x]. *)

val write : ?eq:('a -> 'a -> bool) -> 'a -> 'a cc
(** [write x] is a computation that always returns [x].
    @param eq If [eq old_value x = true] then the new value is considered equal
              and no update is done. The default is [(==)].
              It is always safe (though inefficient) to return [false] here. *)

val of_cc : 'a cc -> 'a t
(** [of_cc x] is a changeable value holding the result of evaluating [x]. *)

val on_release : (unit -> unit) -> unit
(** [on_release fn] calls [fn] if the current computation has to be redone.
    This may be useful to release external resources when they are no longer needed.
    Note that the order in which multiple such functions are called is somewhat
    unpredictable. *)

(** {2 External operations}

    These functions are used to interface between the changeable system and other systems (e.g. Lwt). *)

type 'a var
(** A mutable value that can be changed using [change]. Internally, [type 'a var = 'a t],
    but it's useful to distinguish between values that are inputs to the system and
    values that only change in response to computations being rerun. *)

val var : 'a -> 'a var
(** [var x] creates a new variable initially set to [x]. *)

val of_var : 'a var -> 'a t
(** [of_var x] casts [x] to ['a t]. *)

val change : ?eq:('a -> 'a -> bool) -> 'a var -> 'a -> unit
(** [change x v] sets the current value of [x] to [v] and adds anything
    that depends on it to the rebuild queue. You must call [propagate] after
    this to update everything (you can change several things together and then
    call [propagate] once).
    This function can only be used from the top-level, not from within a computation. *)

val propagate : unit -> unit
(** Apply all changes made with [change], so that everything is up-to-date. *)

val observe : 'a t -> 'a
(** [observe t] is the current value of [t]. If [change] has been used then the value
    may be stale until [propagate] is called. *)
