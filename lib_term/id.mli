type t
val mint : unit -> t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
