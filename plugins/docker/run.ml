type t = No_context

let id = "docker-run"

module Key = struct
  type t = {
    image : Image.t;
    args : string list;
    docker_host : string option;
  }

  let pp_args = Fmt.(list ~sep:sp (quote string))

  let cmd { image; args; docker_host } =
    Cmd.docker ~docker_host @@ ["run"; "-i"; Image.hash image] @ args

  let pp f t = Cmd.pp f (cmd t)

  let digest { image; args; docker_host } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
      "args", [%derive.to_yojson:string list] args;
      "docker_host", [%derive.to_yojson:string option] docker_host;
    ]
end

module Value = Current.Unit

let build ~switch No_context job key =
  Current.Process.exec ~switch ~job (Key.cmd key)

let pp = Key.pp

let auto_cancel = true

let level _ _ = Current.Level.Average
