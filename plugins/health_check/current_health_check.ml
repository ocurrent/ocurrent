open Current.Syntax

let src = Logs.Src.create "current.health.check" ~doc:"OCurrent health check plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module Address = Address

module D = Current_cache.Make(Dig)

let dig ~schedule ~fqdn =
  Current.component "dig %s" fqdn |>
  let> () = Current.return () in
  D.get ~schedule Dig.No_context { Dig.Key.fqdn }

module C = Current_cache.Output(Curl)

let curl ~schedule ~address =
  Current.component "curl" |>
  let> address = address in
  C.set ~schedule Curl.No_context address ()

module P = Current_cache.Output(Ping)

let ping ~schedule ~address =
  Current.component "ping" |>
  let> address = address in
  P.set ~schedule Ping.No_context address ()

let expand x = Address.expand x

