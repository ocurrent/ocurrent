open Lwt.Infix

type t = No_context

let ( >>!= ) = Lwt_result.bind

module Key = struct
  type t = {
    docker_context : string option;
    arch: string;
    tag : string;
  } [@@deriving to_yojson]

  let cmd { docker_context; tag; _ } = Cmd.docker ~docker_context ["manifest"; "inspect"; "--"; tag]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Current.String

let id = "docker-peek"

let build No_context job key =
  Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
  let { Key.docker_context = _; tag; arch } = key in
  Current.Process.check_output ~cancellable:true ~job (Key.cmd key) >>!= fun manifest ->
  match Pull.get_digest_from_manifest manifest arch with
  | Error _ as e -> Lwt.return e
  | Ok hash ->
    Current.Job.log job "Got %S" hash;
    Prometheus.Counter.inc_one Metrics.docker_peek_events;
    Lwt_result.return (tag ^ "@" ^ hash)

let pp f key = Cmd.pp f (Key.cmd key)

let auto_cancel = true
