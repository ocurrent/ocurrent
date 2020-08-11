open Lwt.Infix

let () =
  Random.self_init ()

let failf fmt = fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let pp_args f args =
  match Array.to_list args with
  | [] -> failwith "empty command!"
  | x :: xs -> Fmt.string f (Filename.quote_command x xs)

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let check_status cmd = function
  | Unix.WEXITED 0 -> Ok ()
  | Unix.WEXITED x -> failf "%t exited with status %d" cmd x
  | Unix.WSIGNALED x -> failf "%t failed with signal %d" cmd x
  | Unix.WSTOPPED x ->
      failf "%t stopped with signal %a" cmd pp_signal x

let make_tmp_dir ?(prefix = "tmp-") ?(mode = 0o700) parent =
  let rec mktmp = function
    | 0 -> Fmt.failwith "Failed to generate temporary directroy name!"
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

let rm_f_tree root =
  let rec rmtree path =
    Lwt_unix.lstat path >>= fun info ->
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
      Lwt_unix.unlink path
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

let pp_command cmd f = Fmt.pf f "Command %a" pp_cmd cmd

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

let exec ?cwd ?(stdin="") ?pp_error_command ~cancellable ~job cmd =
  let cwd = Option.map Fpath.to_string cwd in
  let pp_error_command = Option.value pp_error_command ~default:(pp_command cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let proc = Lwt_process.open_process ?cwd ~stderr:(`FD_copy Unix.stdout) cmd in
  let copy_thread = copy_to_log ~job proc#stdout in
  add_shutdown_hooks ~cancellable ~job ~cmd proc >>= fun () ->
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  proc#status >|= fun status ->
  match check_status pp_error_command status with
  | Ok () -> stdin_result
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?pp_error_command ~cancellable ~job cmd =
  let cwd = Option.map Fpath.to_string cwd in
  let pp_error_command = Option.value pp_error_command ~default:(pp_command cmd) in
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
  match check_status pp_error_command status with
  | Error _ as e -> e
  | Ok () ->
    match stdin_result with
    | Error _ as e -> e
    | Ok () ->
      Ok stdout
