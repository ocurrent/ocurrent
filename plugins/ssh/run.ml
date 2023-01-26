open Lwt.Infix

type t = string

let id = "ssh-run"

module Key = Current.String
module Value = struct
  type t = {
    args: string list;
  }

  let digest { args } =
    Yojson.Safe.to_string @@ `Assoc [
      "args", `String (String.concat " " args)
    ]
end
module Outcome = Current.Unit

let command ~ssh_host args =
  let cmd = ["ssh"; ssh_host] @ args in
  ("", Array.of_list cmd)

let publish t job _key { Value.args } =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Process.exec ~cancellable:true ~job (command ~ssh_host:t args)

let pp f (key, { Value.args }) = Fmt.pf f "%s: %s" key (String.concat " " args)

let auto_cancel = false
