type 'a t = 'a Output.t

let return x = Ok x
let fail msg = Error (`Msg msg)

let bind x f =
  match x with
  | Error _ as e -> e
  | Ok y -> f y

let map f x =
  match x with
  | Error _ as e -> e
  | Ok y -> Ok (f y)

let pair a b =
  match a, b with
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok x, Ok y -> Ok (x, y)

let pending = Error `Pending

let run x = x

let pp ok f = function
  | Ok x -> ok f x
  | Error `Pending -> Fmt.string f "(pending)"
  | Error `Msg m -> Fmt.pf f "FAILED: %s" m
