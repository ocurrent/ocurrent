open Lwt.Infix

type t = No_context

let id = "curl"

module Key = Address

module Value = Current.Unit

module Outcome = Current.Unit

let publish No_context job key _ =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let ip = match key.Key.ver with
        | V6 -> "[" ^ key.Key.ip ^ "]"
        | V4 -> key.Key.ip in
  Current.Process.exec ~cancellable:true ~job ("", [| "curl"; "-L"; "--resolve"; key.Key.fqdn ^ ":443:" ^ ip; "https://" ^ key.Key.fqdn |])

let pp f (_, _) =
  Fmt.pf f "curl"

let auto_cancel = false
