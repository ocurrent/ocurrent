(* Utilties for generating dot files. *)

val node : Format.formatter -> ?style:string -> ?shape:string -> ?bg:string -> ?url:string -> int -> string -> unit
val edge : Format.formatter -> ?style:string -> ?color:string -> int -> int -> unit

val begin_cluster : Format.formatter -> int -> unit
val end_cluster : Format.formatter -> unit
