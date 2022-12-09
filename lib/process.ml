open Lwt.Infix

let () =
  Random.self_init ()

let failf fmt = fmt |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(array ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_command pp_cmd cmd f = Fmt.pf f "Command %a" pp_cmd cmd

let check_status ?pp_error_command cmd status =
  let pp_cmd = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED 127 ->
      let cmd_name =
        match cmd with
        | "", args ->
            if Array.length args > 0 then Some (Array.get args 0) else None
        | p, _ -> Some p
      in
      if Option.is_some cmd_name then
        failf "%t exited with status %d. Is %s installed?" pp_cmd 127
          (Option.get cmd_name)
      else failf "%t exited with status %d" pp_cmd 127
  | Unix.WEXITED x -> failf "%t exited with status %d" pp_cmd x
  | Unix.WSIGNALED x -> failf "%t failed with signal %d" pp_cmd x
  | Unix.WSTOPPED x -> failf "%t stopped with signal %a" pp_cmd pp_signal x

let make_tmp_dir ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> Fmt.failwith "Failed to generate temporary directory name!"
    | n -> (
      let tmppath =
        Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff)
      in
      try
        Unix.mkdir tmppath mode;
        tmppath
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        Log.warn (fun f -> f "Temporary directory %s already exists!" tmppath);
        mktmp (n - 1) )
  in
  mktmp 10

let win32_unlink fn =
  Lwt.catch
    (fun () -> Lwt_unix.unlink fn)
    (function
     | Unix.Unix_error (Unix.EACCES, _, _) as exn ->
        (* Try removing the read-only attribute before retrying unlink. We catch
          any exception here and ignore it in favour of the original [exn]. *)
        Lwt.catch
          (fun () ->
            Lwt_unix.lstat fn >>= fun {st_perm; _} ->
            Lwt_unix.chmod fn 0o666 >>= fun () ->
            Lwt.catch
              (fun () -> Lwt_unix.unlink fn)
              (function _ ->
                 (* If everything succeeded but the final removal still failed,
                   restore original permissions *)
                 Lwt_unix.chmod fn st_perm >>= fun () ->
                 Lwt.fail exn)
          )
          (fun _ -> Lwt.fail exn)
     | exn -> Lwt.fail exn)

let unlink =
  if Sys.win32 then
    win32_unlink
  else
    Lwt_unix.unlink

let rm_f_tree root =
  let rec rmtree path =
    Lwt_unix.lstat path >>= fun info ->
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
      unlink path
    | Unix.S_DIR ->
      Lwt_unix.chmod path 0o700 >>= fun () ->
      Lwt_unix.files_of_directory path
      |> Lwt_stream.iter_s (function
          | "." | ".." -> Lwt.return_unit
          | leaf -> rmtree (Filename.concat path leaf)
        )
      >>= fun () ->
      Lwt_unix.rmdir path
  in
  rmtree root

let with_tmpdir ?prefix fn =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  Lwt.finalize
    (fun () -> fn (Fpath.v tmpdir))
    (fun () -> rm_f_tree tmpdir)

let send_to ch contents =
  Lwt.try_bind
    (fun () ->
       Lwt_io.write ch contents >>= fun () ->
       Lwt_io.close ch
    )
    (fun () -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Error (`Msg (Printexc.to_string ex))))

let copy_to_log ~job src =
  let rec aux () =
    Lwt_io.read ~count:4096 src >>= function
    | "" -> Lwt.return_unit
    | data -> Job.write job data; aux ()
  in
  aux ()

let add_shutdown_hooks ~cancellable ~job ~cmd proc =
  if cancellable then (
    Job.on_cancel job (fun reason ->
        if proc#state = Lwt_process.Running then (
          Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
          proc#terminate;
        );
        Lwt.return_unit
      )
  ) else (
    (* Always terminate process if the job ends: *)
    Switch.add_hook_or_exec job.Job.switch (fun _reason ->
        if proc#state = Lwt_process.Running then proc#terminate;
        Lwt.return_unit
      )
  )

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  let cwd = Option.map Fpath.to_string cwd in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let proc = Lwt_process.open_process ?cwd ~stderr:(`FD_copy Unix.stdout) cmd in
  let copy_thread = copy_to_log ~job proc#stdout in
  add_shutdown_hooks ~cancellable ~job ~cmd proc >>= fun () ->
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  proc#status >|= fun status ->
  match check_status ?pp_error_command cmd status with
  | Ok () -> stdin_result
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  let cwd = Option.map Fpath.to_string cwd in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let proc = Lwt_process.open_process_full ?cwd cmd in
  let copy_thread = copy_to_log ~job proc#stderr in
  add_shutdown_hooks ~cancellable ~job ~cmd proc >>= fun () ->
  let reader = Lwt_io.read proc#stdout in
  send_to proc#stdin stdin >>= fun stdin_result ->
  reader >>= fun stdout ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  proc#status >|= fun status ->
  match check_status ?pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () ->
    match stdin_result with
    | Error _ as e -> e
    | Ok () ->
      Ok stdout
