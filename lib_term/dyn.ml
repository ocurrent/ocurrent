type 'a t = 'a Output.t

let return x = Ok x
let fail msg = Error (`Msg msg)
let of_output x = x

let state x = Ok x

let catch = function
  | Ok _ | Error (`Msg _) as x -> Ok x
  | Error (`Active _) as x -> x

let bind x f =
  match x with
  | Error _ as e -> e
  | Ok y -> f y

let map f x =
  match x with
  | Error _ as e -> e
  | Ok y ->
    try Ok (f y)
    with ex -> Error (`Msg (Printexc.to_string ex))

let pair a b =
  match a, b with
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok x, Ok y -> Ok (x, y)

let active a = Error (`Active a)

let run x = x

let pp ok f = function
  | Ok x -> ok f x
  | Error (`Active `Ready) -> Fmt.string f "(ready)"
  | Error (`Active `Running) -> Fmt.string f "(running)"
  | Error `Msg m -> Fmt.pf f "FAILED: %s" m
