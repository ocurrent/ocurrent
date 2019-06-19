type 'a t = ('a, [`Pending | `Msg of string]) result
  [@@deriving eq]

let pp ok f = function
  | Ok x -> Fmt.pf f "Ok: %a" ok x
  | Error `Pending -> Fmt.string f "Pending"
  | Error (`Msg e) -> Fmt.pf f "Error: %s" e
