open Lwt.Infix

type t = No_context

module Key = Current.String
module Value = Image

let id = "docker-pull"

let build ~switch No_context job image =
  let cmd = Array.of_list @@ ["docker"; "pull"; image] in
  Current_cache.Process.exec ~switch ~job ("", cmd) >|= function
  | Ok () -> Ok image
  | Error _ as e -> e

let pp f x = Fmt.pf f "docker pull %s" x

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
