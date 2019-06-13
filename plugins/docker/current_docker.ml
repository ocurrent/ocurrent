open Lwt.Infix
open Current.Syntax

let src = Logs.Src.create "current.docker" ~doc:"OCurrent docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Current_git.Commit.t

module Image = struct
  type t = string
end

module Builder = struct
  type t = No_context
  module Key = Current_git.Commit
  module Value = Image

  let docker_tag t = Fmt.strf "build-of-%s" (Key.id t)

  let build ~switch:_ No_context commit =
    let tag = docker_tag commit in
    Current_git.with_checkout commit @@ fun dir ->
    let cmd = [| "docker"; "build"; "-t"; tag; "--"; Fpath.to_string dir |] in
    let proc = Lwt_process.open_process_none ("", cmd) in
    proc#status >|= function
    | Unix.WEXITED 0 ->
      Log.info (fun f -> f "Build of docker image %S succeeded" tag);
      Ok tag
    | Unix.WEXITED n ->
      Error (`Msg (Fmt.strf "Build failed with exit status %d" n))
    | Unix.WSIGNALED s ->
      Error (`Msg (Fmt.strf "Build failed with signal %d" s))
    | Unix.WSTOPPED x ->
      Error (`Msg (Fmt.strf "Expected exit status: stopped with %d" x))

  let pp f key = Fmt.pf f "docker build %a" Key.pp key

  let auto_cancel = true
end

module C = Current_cache.Make(Builder)

let build src =
  "build" |>
  let** src = src in
  C.get Builder.No_context src
