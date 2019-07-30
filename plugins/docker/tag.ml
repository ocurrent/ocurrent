open Lwt.Infix

type auth = string * string

type t = auth option

let ( >>!= ) = Lwt_result.bind

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
    op : [`Tag | `Push];  (* Tag on [docker_host] or push to hub. *)
  }

  let pp_op f = function
    | `Push -> Fmt.string f "push"
    | `Tag -> Fmt.string f "tag"

  let digest { image; op } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
      "op", `String (Fmt.to_to_string pp_op op);
    ]
end

let tag_cmd { Key.tag; docker_host } { Value.image; op = _ } =
  Cmd.docker ~docker_host ["tag"; Image.hash image; tag]

let publish ~switch auth job key value =
  Current.Process.exec ~switch ~job (tag_cmd key value) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    match value.Value.op with
    | `Tag -> Lwt.return @@ Ok ()
    | `Push ->
      let { Key.tag; docker_host } = key in
      begin match auth with
        | None -> Lwt.return (Ok ())
        | Some (user, password) ->
          let cmd = Cmd.login ~docker_host ~user in
          Current.Process.exec ~switch ~job ~stdin:password cmd
      end >>!= fun () ->
      let cmd = Cmd.docker ~docker_host ["push"; tag] in
      Current.Process.exec ~switch ~job cmd

let pp f (key, value) =
  Cmd.pp f (tag_cmd key value);
  match value.op with
  | `Tag -> ()
  | `Push -> Fmt.pf f "; docker push %S" key.Key.tag

let auto_cancel = false

let level _auth _tag value =
  match value.Value.op with
  | `Tag -> Current.Level.Average
  | `Push -> Current.Level.Dangerous
