(** Utilities for retrying Lwt computations *)

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]
(** The type of errors that a retryable computation can produce.

    - [`Retry r] when [r] represents an error that can be retried.
    - [`Fatal f] when [f] represents an error that cannot be retried. *)

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error) result
(** A [('ok, 'retry, 'fatal) attempt] is an alias for the [result] of a
    retryable computation.

    - [Ok v] produces a successful value [v]
    - [Error err] produces the {!type:error} [err] *)

val pp_error :
  ?retry:'retry Fmt.t ->
  ?fatal:'fatal Fmt.t ->
  ('retry, 'fatal) error Fmt.t
(** [pp_error ~retry ~fatal] is a formatter for {!type:error}s that formats
    fatal and retryable errors according to the provided formatters.

    If either formatter is not provided, a default formatter will represent the
    values as ["<opaque>"]. *)

val equal_error :
  retry:('retry -> 'retry -> bool) ->
  fatal:('fatal -> 'fatal -> bool) ->
  ('retry, 'fatal) error ->
  ('retry, 'fatal) error ->
  bool

val on_error :
  (unit -> ('ok, 'retry, 'fatal) attempt Lwt.t) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [on_error f] is a stream of attempts to compute [f]. The stream will continue until
    the computation succeeds or produces a fatal error.

    Examples

    {[
      # open Current;;

      # let success () = Lwt.return_ok ();;
      val success : unit -> (unit, 'a) result Lwt.t = <fun>
      # Retry.(success |> on_error) |> Lwt_stream.to_list;;
      - : (unit, 'a, 'b) Current.Retry.attempt list = [Ok ()]

      # let fatal_failure () = Lwt.return_error (`Fatal ());;
      val fatal_failure : unit -> ('a, [> `Fatal of unit ]) result Lwt.t = <fun>
      # Retry.(fatal_failure |> on_error) |> Lwt_stream.to_list;;
      - : ('a, 'b, unit) Current.Retry.attempt list = [Error (`Fatal ())]

      # let retryable_error () = Lwt.return_error (`Retry ());;
      val retryable_error : unit -> ('a, [> `Retry of unit ]) result Lwt.t = <fun>
      # Retry.(retryable_error |> on_error) |> Lwt_stream.nget 3;;
      - : ('a, unit, 'b) Current.Retry.attempt list =
      [Error (`Retry ()); Error (`Retry ()); Error (`Retry ())]
    ]}*)

val with_sleep :
  ?duration:(int -> float) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [with_sleep ~duration attempts] is the stream of [attempts] with a sleep
    added after computing each [n]th retryable attempt based on [duration n].

    @param duration the optional sleep duration. This defaults to an exponential
    backoff computed as n * 2 * (1.5 ^ n), which gives the approximate sequence 0s -> 3s ->
    9s -> 20.25 -> 40.5s -> 75.9s -> 136.7 -> ...

    Examples
    {[
      # let retryable_error () = Lwt.return_error (`Retry ());;
      # let attempts_with_sleeps = Retry.(retryable_error |> on_error |> with_sleep);;
      # Lwt_stream.get attempts_with_sleeps;;
      (* computed immediately *)
      Some (Error (`Retry ()))

      # Lwt_stream.get attempts_with_sleeps;;
      (* after 3 seconds *)
      Some (Error (`Retry ()))

      # Lwt_stream.get attempts_with_sleeps;;
      (* after 9 seconds *)
      Some (Error (`Retry ()))

      (* a stream with a constant 1s sleep between attempts *)
      # let attempts_with_constant_sleeps =
          Retry.(retryable_error |> on_error |> with_sleep ~duration:(fun _ -> 1.0));;
    ]} *)

val n_times :
  ?pp:('retry, 'fatal) error Fmt.t ->
  int ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, [`Msg of string]) result Lwt.t

(** [n_times n attempts] is [Ok v] if one of the [attempts] succeeds within [n]
    retries. Otherwise, it is [Error (`Msg msg)] with [msg] derived based on the
    error formatter.

    @param pp an optional formatter used to produce the error message,
    defaulting to {!pp_error}.

    Examples

    {[
      # let operation () =
        let i = ref 0 in
        fun () -> Lwt.return_error (if !i < 3 then (incr i; `Retry !i) else `Fatal "msg");;
      # Retry.(operation () |> on_error |> n_times ~pp:(pp_error ~retry:Fmt.int ~fatal:Fmt.string) 5);;
      - : ('a, [ `Msg of string ]) result = Error (`Msg "fatal error 'msg' (fatal)")
    ]} *)
