open Lwt.Infix
open Current.Syntax

let src = Logs.Src.create "current.docker" ~doc:"OCurrent docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Current_git.Commit.t

module Image = struct
  type t = string
  let pp = Fmt.string
end

module Build = struct
  type t = No_context
  module Key = Current_git.Commit
  module Value = Image

  let docker_tag t = Fmt.strf "build-of-%s" (Key.id t)

  let build ~switch No_context commit =
    let tag = docker_tag commit in
    Current_git.with_checkout commit @@ fun dir ->
    let cmd = [| "docker"; "build"; "-t"; tag; "--"; Fpath.to_string dir |] in
    let proc = Lwt_process.open_process_none ("", cmd) in
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        if proc#state = Lwt_process.Running then (
          Log.info (fun f -> f "Cancelling build of %a" Key.pp commit);
          proc#terminate;
        );
        Lwt.return_unit
      )
    >>= fun () ->
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

  let level _ _ = Current.Level.Average
end

module BC = Current_cache.Make(Build)

let build src =
  "build" |>
  let** src = src in
  BC.get Build.No_context src

module Unit = struct
  type t = unit
end

module Run = struct
  type t = No_context
  module Key = struct
    type t = Image.t * string list

    let pp_args = Fmt.(list ~sep:sp (quote string))
    let pp f (image, args) = Fmt.pf f "docker run @[%a %a@]" Image.pp image pp_args args
    let compare = compare
  end
  module Value = Unit

  let build ~switch No_context key =
    let (image, args) = key in
    let cmd = Array.of_list @@ ["docker"; "run"; "-i"; image] @ args in
    let proc = Lwt_process.open_process_none ("", cmd) in
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        if proc#state = Lwt_process.Running then (
          Log.info (fun f -> f "Cancelling run of %a" Key.pp key);
          proc#terminate;
        );
        Lwt.return_unit
      )
    >>= fun () ->
    proc#status >|= function
    | Unix.WEXITED 0 -> Ok ()
    | Unix.WEXITED n ->
      Error (`Msg (Fmt.strf "Docker run failed with exit status %d" n))
    | Unix.WSIGNALED s ->
      Error (`Msg (Fmt.strf "Docker run failed with signal %d" s))
    | Unix.WSTOPPED x ->
      Error (`Msg (Fmt.strf "Expected exit status: stopped with %d" x))

  let pp = Key.pp

  let auto_cancel = true

  let level _ _ = Current.Level.Average
end

module RC = Current_cache.Make(Run)

let run image ~args =
  "run" |>
  let** image = image in
  RC.get Run.No_context (image, args)
