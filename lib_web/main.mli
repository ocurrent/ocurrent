open Tyxml.Html

(** Very primitive CSRF protection. *)
val csrf_token : string

val template : [< Html_types.div_content_fun ] elt list_wrap -> string
val dashboard : Current.Engine.t -> string
