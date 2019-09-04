open Lwt.Infix

type t = No_context

module Key = struct
  type t = {
    docker_context : string option;
    tag : string;
  } [@@deriving to_yojson]

  let cmd { docker_context; tag } = Cmd.docker ~docker_context ["pull"; tag]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Image

let id = "docker-pull"

let build ~switch No_context job key =
  Current.Job.set_running job;
  Current.Process.exec ~switch ~job (Key.cmd key) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    let { Key.docker_context; tag } = key in
    let cmd = Cmd.docker ~docker_context ["image"; "inspect"; tag; "-f"; "{{index .RepoDigests 0}}"] in
    Current.Process.check_output ~job cmd >|= function
    | Error _ as e -> e
    | Ok id ->
      let id = String.trim id in
      Current.Job.log job "Pulled %S -> %S" tag id;
      Ok (Image.of_hash id)

let pp f key = Cmd.pp f (Key.cmd key)

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
