type t = No_context

let id = "docker-run"

module Key = struct
  type t = {
    image : Image.t;
    args : string list;
    docker_context : string option;
  }

  let pp_args = Fmt.(list ~sep:sp (quote string))

  let cmd { image; args; docker_context } =
    Cmd.docker ~docker_context @@ ["run"; "-i"; Image.hash image] @ args

  let pp f t = Cmd.pp f (cmd t)

  let digest { image; args; docker_context } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
      "args", [%derive.to_yojson:string list] args;
      "docker_context", [%derive.to_yojson:string option] docker_context;
    ]
end

module Value = Current.Unit

let build ~switch ~set_running No_context job key =
  set_running ();
  Current.Process.exec ~switch ~job (Key.cmd key)

let pp = Key.pp

let auto_cancel = true

let level _ _ = Current.Level.Average
