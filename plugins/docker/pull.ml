open Lwt.Infix

type t = No_context

module Key = Current.String
module Value = Image

let id = "docker-pull"

let build ~switch No_context job tag =
  let cmd = Array.of_list @@ ["docker"; "pull"; tag] in
  Current_cache.Process.exec ~switch ~job ("", cmd) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    let cmd = [| "docker"; "image"; "inspect"; tag; "-f"; "{{.Id}}" |] in
    Lwt_process.pread_line ("", cmd) >|= fun id ->
    Current_cache.Job.log job "Pulled %S -> %S" tag id;
    Ok id

let pp f x = Fmt.pf f "docker pull %s" x

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
