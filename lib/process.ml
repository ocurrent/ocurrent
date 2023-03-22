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

let check_status pp_cmd cmd = function
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
  try
    Unix.unlink fn
  with
  | Unix.Unix_error (Unix.EACCES, _, _) as exn ->
    try
      let st_perm = (Unix.lstat fn).st_perm in
      Unix.chmod fn 0o666;
      try
        Unix.unlink fn
      with
      | _ ->
        (* If everything succeeded but the final removal still failed,
          restore original permissions *)
        Unix.chmod fn st_perm;
        raise exn
    with
    | _ -> raise exn

let unlink =
  if Sys.win32 then
    win32_unlink
  else
    Unix.unlink

let get_files_in_dir path =
  let handle = Unix.opendir path in
  let rec aux () =
    try Unix.readdir handle :: aux ()
    with Not_found -> []
  in
  aux ()

let rm_f_tree root : unit =
  let rec rmtree path =
    let info = Unix.lstat path in
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
      unlink path
    | Unix.S_DIR ->
      Unix.chmod path 0o700;
      get_files_in_dir path
      |> List.iter (function
          | "." | ".." -> ()
          | leaf -> rmtree (Filename.concat path leaf)
        );
      Unix.rmdir path
  in
  rmtree root

let with_tmpdir ?prefix fn =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  fn (Fpath.v tmpdir);
  rm_f_tree tmpdir

let send_to ch contents =
  try
    let len = String.length contents in
    let buf = Bytes.of_string contents in
    let _ = Unix.write ch buf 0 len in
    Unix.close ch;
    Ok ()
  with
  | ex -> Error (`Msg (Printexc.to_string ex))

let pp_command pp_cmd cmd f = Fmt.pf f "Command %a" pp_cmd cmd

let copy_to_log ~job src =
  let size = 4096 in
  let buf = Bytes.create size in
  let rec aux () =
    match Unix.read src buf 0 size with
    | 0 -> ()
    | n when n = size -> Job.write job (Bytes.to_string buf); aux ()
    | n ->
      Bytes.blit buf 0 buf 0 n;
      Job.write job (Bytes.to_string buf) 
  in
  aux ()

(* TODO: Lwt's implementation is by some external function *)
let win32_terminate _proc = ()

let unix_terminate proc =
  let pid = Unix.process_full_pid proc in
  Unix.kill pid Sys.sigkill

type state =
  | Running
  | Exited of Unix.process_status

(* TODO: Lwt's implementation uses Lwt.poll *)
let win32_state _proc = Running

let unix_state proc =
  let pid = Unix.process_full_pid proc in
  (* With WNOHANG, if the process is running then immediately return with a pid of 0 *)
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ -> Running
  | _, s -> Exited s

(* TODO: Lwt's implementation uses some external function! *)
let win32_waitproc _proc = Lwt_unix.WEXITED 1
  (* Lwt_unix.run_job (win32_wait_job proc.fd) >>= fun code ->
  Lwt.return
    (proc.id,
      Lwt_unix.WEXITED code,
      {Lwt_unix.ru_utime = 0.; Lwt_unix.ru_stime = 0.}) *)

let unix_waitproc proc =
  let pid = Unix.process_full_pid proc in
  Unix.waitpid [] pid
  |> snd

let terminate = if Sys.win32 then win32_terminate else unix_terminate
let state = if Sys.win32 then win32_state else unix_state
let waitproc  = if Sys.win32 then win32_waitproc else unix_waitproc

let add_shutdown_hooks ~cancellable ~job ~cmd proc =
  Lwt_eio.run_lwt @@ fun () ->
    if cancellable then (
      Job.on_cancel job (fun reason ->
          if state proc = Running then (
            Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
            terminate proc;
          );
          Lwt.return_unit
        )
    ) else (
      (* Always terminate process if the job ends: *)
      Switch.add_hook_or_exec job.Job.switch (fun _reason ->
          if state proc = Running then terminate proc;
          Lwt.return_unit
        )
    )

let normalise_process (proc_out, proc_in, proc_err) =
  let proc_out = Unix.descr_of_in_channel proc_out in
  let proc_in = Unix.descr_of_out_channel proc_in in
  let proc_err = Unix.descr_of_in_channel proc_err in
  (proc_out, proc_in, proc_err)

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  (* let cwd = Option.map Fpath.to_string cwd in *)
  ignore cwd;
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let cmd_str = String.concat " " @@ Array.to_list (snd cmd) in
  let proc = Unix.open_process_full cmd_str [||] in
  let (proc_out, proc_in, _) = normalise_process proc in
  let _, stdin_result =
    Eio.Fiber.pair
      (fun () -> copy_to_log ~job proc_out)
      (fun () ->
        add_shutdown_hooks ~cancellable ~job ~cmd proc;
        send_to proc_in stdin)
  in
  let status = waitproc proc in
  match check_status pp_error_command cmd status with
  | Ok () -> stdin_result
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job cmd =
  (* let cwd = Option.map Fpath.to_string cwd in *)
  ignore cwd;
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let cmd_str = String.concat " " @@ Array.to_list (snd cmd) in
  let proc = Unix.open_process_full cmd_str [||] in
  let (proc_out, proc_in, proc_err) = normalise_process proc in
  copy_to_log ~job proc_err;
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let buf = Bytes.create 4096 in
  let len = Unix.read proc_out buf 0 4096 in
  Bytes.blit buf 0 buf 0 len;
  let stdout = Bytes.to_string buf in
  let stdin_result = send_to proc_in stdin in
  let status = waitproc proc in
  match check_status pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () ->
    match stdin_result with
    | Error _ as e -> e
    | Ok () ->
      Ok stdout
