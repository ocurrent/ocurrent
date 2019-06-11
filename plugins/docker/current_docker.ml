open Lwt.Infix
open Current.Syntax

let src = Logs.Src.create "current.docker" ~doc:"OCurrent docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Fpath.t

module Image = struct
  type t = string
end

module Key : sig
  include Set.OrderedType
  val of_path : Fpath.t -> t
  val dir : t -> Fpath.t
  val docker_tag : t -> string
  val pp : t Fmt.t
end = struct
  type t = Fpath.t

  let of_path t =
    if Fpath.is_abs t then t
    else Fpath.normalize @@ Fpath.append (Fpath.v @@ Sys.getcwd ()) t

  let compare = Fpath.compare

  let pp = Fpath.pp

  let dir t = t

  let docker_tag t =
    let hash = Fpath.to_string t |> Digest.string |> Digest.to_hex in
    Fmt.strf "build-of-%s-%s" (Fpath.basename t) hash
end

module Builder = struct
  type t = No_context
  module Key = Key
  module Value = Image

  let build ~switch:_ No_context key =
    let tag = Key.docker_tag key in
    let cmd = [| "docker"; "build"; "-t"; tag; "--"; Fpath.to_string (Key.dir key) |] in
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

let build c =
  "build" |>
  let** c = c in
  C.get Builder.No_context @@ Key.of_path c
