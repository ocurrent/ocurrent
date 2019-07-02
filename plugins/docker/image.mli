include Current_cache.S.WITH_DIGEST
include Current_cache.S.WITH_MARSHAL with type t := t

val of_hash : string -> t
val hash : t -> string
val pp : t Fmt.t
