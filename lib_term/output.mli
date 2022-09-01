type active = [`Ready | `Running | `Waiting_for_confirmation]
  [@@deriving eq]

type 'a t = ('a, [`Active of active | `Msg of string]) result
  [@@deriving eq]

val pp : 'a Fmt.t -> ('a, [< `Active of active | `Msg of string]) result Fmt.t

module Blockable : sig

  type 'a t = ('a, [`Active of active | `Msg of string | `Blocked]) result
  [@@deriving eq]

  val pp : 'a Fmt.t -> 
    ('a, [< `Active of active | `Msg of string | `Blocked]) result Fmt.t

end
