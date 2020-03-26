type 'a t = ('a, Id.t * [`Active of Output.active | `Msg of string]) result

let return x = Ok x
let fail ~id msg = Error (id, `Msg msg)

let strip_id = function
  | Ok x -> Ok x
  | Error (_, e) -> Error e

let state x = Ok (strip_id x)

let catch = function
  | Ok _ as x -> Ok x
  | Error (_, (`Msg _ as x)) -> Ok (Error x)
  | Error (_, `Active _) as x -> x

let msg_of_exn = function
  | Failure m -> m
  | ex -> Printexc.to_string ex

let bind x f =
  match x with
  | Error _ as e -> e
  | Ok y -> f y

let map ~id f x =
  match x with
  | Error _ as e -> e
  | Ok y ->
    match f y with
    | y -> Ok y
    | exception ex -> Error (id, `Msg (msg_of_exn ex))

let map_error ~id f x =
  match x with
  | Error (_, `Msg m) ->
    let m = try f m with ex -> msg_of_exn ex in
    Error (id, `Msg m)
  | _ -> x

let pair a b =
  match a, b with
  | (Error (_, `Msg _) as e), _
  | _, (Error (_, `Msg _) as e) -> e
  | (Error (_, `Active _) as e), _
  | _, (Error (_, `Active _) as e) -> e
  | Ok x, Ok y -> Ok (x, y)

let active ~id a = Error (id, `Active a)

let run = strip_id

let pp ok f = function
  | Ok x -> ok f x
  | Error (_, `Active `Ready) -> Fmt.string f "(ready)"
  | Error (_, `Active `Running) -> Fmt.string f "(running)"
  | Error (_, `Msg m) -> Fmt.pf f "FAILED: %s" m
