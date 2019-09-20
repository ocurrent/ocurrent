open Lwt.Infix

let () =
  Random.self_init ()

let failf fmt = fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

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
    let info = Unix.lstat path in
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
        Unix.unlink path
    | Unix.S_DIR ->
        Unix.chmod path 0o700;
        Sys.readdir path
        |> Array.iter (fun leaf -> rmtree (Filename.concat path leaf));
        Unix.rmdir path
  in
  rmtree root

let with_tmpdir ?prefix fn =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  Lwt.finalize
    (fun () -> fn (Fpath.v tmpdir))
    (fun () ->
      rm_f_tree tmpdir;
      Lwt.return () )

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
  Lwt.catch aux
    (fun ex ->
       Log.warn (fun f -> f "copy_to_log: %a" Fmt.exn ex);
       Lwt.return_unit
    )

let exec ?switch ?(stdin="") ?pp_error_command ~job cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let proc = Lwt_process.open_process ~stderr:(`FD_copy Unix.stdout) cmd in
  Lwt.async (fun () -> copy_to_log ~job proc#stdout);
  Switch.add_hook_or_exec_opt switch (fun _reason ->
      if proc#state = Lwt_process.Running then (
        Log.info (fun f -> f "Cancelling %a" pp_cmd cmd);
        proc#terminate;
      );
      Lwt.return_unit
    ) >>= fun () ->
  send_to proc#stdin stdin >>= fun stdin_result ->
  proc#status >|= fun status ->
  match check_status pp_error_command status with
  | Ok () -> stdin_result
  | Error _ as e -> e

let check_output ?switch ?cwd ?(stdin="") ?pp_error_command ~job cmd =
  let cwd = Option.map Fpath.to_string cwd in
  let pp_error_command = Option.value pp_error_command ~default:(pp_command cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let proc = Lwt_process.open_process_full ?cwd cmd in
  Lwt.async (fun () -> copy_to_log ~job proc#stderr);
  Switch.add_hook_or_exec_opt switch (fun _reason ->
      if proc#state = Lwt_process.Running then (
        Log.info (fun f -> f "Cancelling %a" pp_cmd cmd);
        proc#terminate;
      );
      Lwt.return_unit
    ) >>= fun () ->
  let reader = Lwt_io.read proc#stdout in
  send_to proc#stdin stdin >>= fun stdin_result ->
  reader >>= fun stdout ->
  proc#status >|= fun status ->
  match check_status pp_error_command status with
  | Error _ as e -> e
  | Ok () ->
    match stdin_result with
    | Error _ as e -> e
    | Ok () ->
      Ok stdout
