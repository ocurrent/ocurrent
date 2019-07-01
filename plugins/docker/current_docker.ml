open Lwt.Infix
open Current.Syntax

type source = Current_git.Commit.t
module Image = Image

module PC = Current_cache.Make(Pull)

let pull ~schedule tag =
  Fmt.strf "pull %s" tag |>
  let** () = Current.return () in
  PC.get ~schedule Pull.No_context tag

module BC = Current_cache.Make(Build)

let pp_sp_label = Fmt.(option (prefix sp string))

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let build ?label ?dockerfile ~pull src =
  Fmt.strf "build%a" pp_sp_label label |>
  let** commit = src
  and* dockerfile = Current.option_seq dockerfile in
  let dockerfile = option_map Dockerfile.string_of_t dockerfile in
  BC.get { Build.pull } { Build.Key.commit; dockerfile }

module Run = struct
  type t = No_context

  let id = "docker-run"

  module Key = struct
    type t = Image.t * string list

    let pp_args = Fmt.(list ~sep:sp (quote string))
    let pp f (image, args) = Fmt.pf f "docker run @[%a %a@]" Image.pp image pp_args args
    let digest (image, args) =
      Fmt.strf "%S %a" (Image.tag image) pp_args args
  end
  module Value = Current.Unit

  let build ~switch No_context job key =
    let (image, args) = key in
    let cmd = Array.of_list @@ ["docker"; "run"; "-i"; image] @ args in
    let log_fd = Current_cache.Job.fd job in
    let stdout = `FD_copy log_fd in
    let stderr = `FD_copy log_fd in
    let proc = Lwt_process.open_process_none ~stdout ~stderr ("", cmd) in
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
