open Lwt.Infix

type t = No_context

module Key = struct
  type t = {
    docker_host : string option;
    tag : string;
  } [@@deriving to_yojson]

  let cmd { docker_host; tag } = Cmd.docker ~docker_host ["pull"; tag]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Image

let id = "docker-pull"

let build ~switch No_context job key =
  Current_cache.Process.exec ~switch ~job (Key.cmd key) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    let { Key.docker_host; tag } = key in
    let cmd = Cmd.docker ~docker_host ["image"; "inspect"; tag; "-f"; "{{.Id}}"] in
    Lwt_process.pread_line cmd >|= function
    | "" -> (* [Lwt_process] is bad at reporting errors *)
      Error (`Msg (Fmt.strf "docker inspect of built image failed!"))
    | id ->
      Current_cache.Job.log job "Pulled %S -> %S" tag id;
      Ok (Image.of_hash id)

let pp f key = Cmd.pp f (Key.cmd key)

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
