type t = { pull : bool; proc : Eio.Process.mgr }

let id = "docker-compose-cli"

module Key = struct
  type t = {
    name: string;
    docker_context: string option;
    detach: bool;
    up_args: string list;
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

let cmd args { Key.docker_context; name; detach=_; up_args=_ } =
  Cmd.docker ~docker_context (["compose"; "-f"; "/dev/stdin"; "-p"; name ] @ args)

let cmd_pull = cmd ["pull"]

let cmd_update ({ Key.detach; up_args; _ } as key) =
  let args = (if detach then ["-d"] else []) @ up_args in
  cmd ("up" :: args) key

let publish { pull; proc } job key {Value.contents} =
  Current.Job.start job ~level:Current.Level.Dangerous;
  let p () =
    if pull then Current.Process.exec ~stdin:contents ~cancellable:true ~job proc (cmd_pull key)
    else (Ok ())
  in
  match p () with
  | Error _ as e -> e
  | Ok () -> Current.Process.exec ~stdin:contents ~cancellable:true ~job proc (cmd_update key)

let pp f (key, { Value.contents }) =
  Fmt.pf f "%a@.@[%a@]" Cmd.pp (cmd_update key) Fmt.string contents

let auto_cancel = false
