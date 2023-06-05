type t = string * Eio.Process.mgr

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
  ("", cmd)

let publish (t, proc) job _key { Value.args } =
  Current.Job.start job ~level:Current.Level.Above_average;
  Current.Process.exec ~cancellable:true ~job proc (command ~ssh_host:t args)

let pp f (key, { Value.args }) = Fmt.pf f "%s: %s" key (String.concat " " args)

let auto_cancel = false
