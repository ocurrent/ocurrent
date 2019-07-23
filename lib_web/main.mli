open Tyxml.Html

val template : [< Html_types.div_content_fun ] elt list_wrap -> string
val dashboard : Current.Engine.results -> string
