open Current.Syntax

let src = Logs.Src.create "current.curl" ~doc:"OCurrent curl plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module Address = Address

module D = Current_cache.Make(Dig)

let resolve ~schedule ~fqdn =
  Current.component "resolve %s" fqdn |>
  let> () = Current.return () in
  D.get ~schedule Dig.No_context { Dig.Key.fqdn }

module C = Current_cache.Output(Curl)

let fetch ~schedule ~address =
  Current.component "curl" |>
  let> address = address in
  C.set ~schedule Curl.No_context address ()

let expand x = Address.expand x

