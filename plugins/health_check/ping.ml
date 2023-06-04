open Lwt.Infix

type t = No_context

let id = "ping"

module Key = Address

module Value = Current.Unit

module Outcome = Current.Unit

let publish No_context job key _ =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  let cmd = match String.contains key.Key.ip ':' with
    | true -> "ping6"
    | false -> "ping" in
  Current.Process.exec ~cancellable:true ~job (cmd, [| cmd; "-c"; "4"; key.Key.ip|])

let pp f (_, _) =
  Fmt.pf f "ping"

let auto_cancel = false
