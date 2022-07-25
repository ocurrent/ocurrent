open Lwt.Infix

type t = No_context

let id = "docker-compose-cli"

module Key = struct
  type t = {
    name : string;
    docker_context : string option;
    detach : bool;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    contents: string;
  }

  let digest { contents } =
    Yojson.Safe.to_string @@ `Assoc [
      "contents", `String contents
    ]
end

module Outcome = Current.Unit

let cmd args { Key.docker_context; name; _ } =
  Cmd.docker ~docker_context (["compose"; "-f"; "/dev/stdin"; "-p"; name ] @ args)

let cmd_update ({ Key.detach; _ } as key) = cmd ("up" :: if detach then ["-d"] else []) key

let publish No_context job key {Value.contents} =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  Current.Process.exec ~stdin:contents ~cancellable:true ~job (cmd_update key)

let pp f (key, { Value.contents }) =
  Fmt.pf f "%a@.@[%a@]" Cmd.pp (cmd_update key) Fmt.string contents

let auto_cancel = false
