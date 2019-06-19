type 'a t = ('a, [`Pending | `Msg of string]) result
  [@@deriving eq]

val pp : 'a Fmt.t -> 'a t Fmt.t
