type 'a t = ('a, [`Pending | `Msg of string]) result
  [@@deriving eq, show]
