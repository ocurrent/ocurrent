let ( >>!= ) = Lwt_result.bind

type t = No_context

let id = "docker-tag"

module Key = struct
  type t = {
    tag : string;
    docker_host : string option;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    image : Image.t;
  }

  let digest { image } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
    ]
end

module Outcome = Current.Unit

let tag_cmd { Key.tag; docker_host } { Value.image } =
  Cmd.docker ~docker_host ["tag"; Image.hash image; tag]

let publish ~switch No_context job key value =
  Current.Process.exec ~switch ~job (tag_cmd key value)

let pp f (key, value) =
  Cmd.pp f (tag_cmd key value)

let auto_cancel = false

let level _auth _tag _value = Current.Level.Average
