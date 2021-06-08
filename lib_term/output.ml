type active = [`Ready | `Running | `Waiting_for_confirmation]
  [@@deriving eq]

type 'a t = ('a, [`Active of active | `Msg of string]) result
  [@@deriving eq]

let pp ok f = function
  | Ok x -> Fmt.pf f "Ok: %a" ok x
  | Error (`Active `Ready) -> Fmt.string f "Ready"
  | Error (`Active `Running) -> Fmt.string f "Running"
  | Error (`Active `Waiting_for_confirmation) -> Fmt.string f "Waiting for confirmation"
  | Error (`Msg e) -> Fmt.pf f "Error: %s" e
