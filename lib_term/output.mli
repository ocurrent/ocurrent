type active = [`Ready | `Running]
  [@@deriving eq]

type 'a t = ('a, [`Active of active | `Msg of string]) result
  [@@deriving eq]

val pp : 'a Fmt.t -> 'a t Fmt.t
