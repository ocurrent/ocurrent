open Lwt.Syntax

let default_sleep_duration n' =
  let base_sleep_time = 2.0 in
  let n = Int.to_float n' in
  n *. base_sleep_time *. Float.pow 1.5 n

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]

let pp_error ?retry ?fatal fmt err =
  let default fmt _ = Fmt.pf fmt "<opaque>" in
  let pp_retry = Option.value retry ~default in
  let pp_fatal = Option.value fatal ~default in
  match err with
  | `Retry r -> Fmt.pf fmt "retryable error '%a'" pp_retry r
  | `Fatal f -> Fmt.pf fmt "fatal error '%a'" pp_fatal f

let equal_error ~retry ~fatal a b =
  match a, b with
  | `Retry a', `Retry b' -> retry a' b'
  | `Fatal a', `Fatal b' -> fatal a' b'
  | _ -> false

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error) result

let is_retryable = function
  | Error (`Retry _) -> true
  | _ -> false

let on_error
    (f : unit -> ('ok, 'retry, 'fatal) attempt Lwt.t)
  : ('ok, 'retry, 'fatal) attempt Lwt_stream.t
  =
  let stop = ref false in
  let attempt () =
    if !stop then
      Lwt.return_none
    else
      let+ result = f () in
      stop := not (is_retryable result);
      Some result
  in
  Lwt_stream.from attempt

let numbered attempts : (int * _) Lwt_stream.t =
  let i = ref 0 in
  let indexes = Lwt_stream.from_direct (fun () -> let n = !i in incr i; Some n) in
  Lwt_stream.combine indexes attempts

let with_sleep ?(duration=default_sleep_duration) attempts =
  attempts
  |> numbered
  |> Lwt_stream.map_s (fun (attempt_number, attempt_result) ->
      let+ () = attempt_number |> duration |> Lwt_unix.sleep in
      attempt_result)

let pp_n_times_error fmt = function
  | `Retry _ -> Fmt.pf fmt "(exhausted)"
  | `Fatal _ -> Fmt.pf fmt "(fatal)"

let n_times ?(pp = (fun e -> pp_error e)) n attempts
  : (_, [`Msg of string]) result Lwt.t
  =
  let to_msg err : [`Msg of string] =
    Fmt.kstr (fun m -> `Msg m) "%a %a" pp err pp_n_times_error err in
  let+ attempts = Lwt_stream.nget n attempts in
  match List.rev attempts with
  | last :: _ -> last |> Result.map_error to_msg (* a [_ Current.or_error] *)
  | _ -> failwith "impossible"
