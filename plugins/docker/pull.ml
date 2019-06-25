open Lwt.Infix

type t = No_context

module Key = String
module Value = Image

let build ~switch No_context job image =
  let cmd = Array.of_list @@ ["docker"; "pull"; image] in
  let log_fd = Current_cache.Job.fd job in
  let stdout = `FD_copy log_fd in
  let stderr = `FD_copy log_fd in
  let proc = Lwt_process.open_process_none ~stdout ~stderr ("", cmd) in
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      if proc#state = Lwt_process.Running then (
        Log.info (fun f -> f "Cancelling pull of %a" Image.pp image);
        proc#terminate;
      );
      Lwt.return_unit
    )
  >>= fun () ->
  proc#status >|= function
  | Unix.WEXITED 0 -> Ok image
  | Unix.WEXITED n ->
    Error (`Msg (Fmt.strf "Docker pull failed with exit status %d" n))
  | Unix.WSIGNALED s ->
    Error (`Msg (Fmt.strf "Docker pull failed with signal %d" s))
  | Unix.WSTOPPED x ->
    Error (`Msg (Fmt.strf "Expected exit status: stopped with %d" x))

let pp f x = Fmt.pf f "docker pull %s" x

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
