open Eio

type command = string * string list

let () =
  Random.self_init ()

let failf fmt = fmt |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.(list ~sep (quote string))

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let check_status pp_cmd cmd = function
  | `Exited 0 -> Ok ()
  | `Exited 127 ->
      let cmd_name =
        match cmd with
        | "", args ->
            if List.length args > 0 then Some (List.hd args) else None
        | p, _ -> Some p
      in
      if Option.is_some cmd_name then
        failf "%t exited with status %d. Is %s installed?" pp_cmd 127
          (Option.get cmd_name)
      else failf "%t exited with status %d" pp_cmd 127
  | `Exited x -> failf "%t exited with status %a" pp_cmd Fmt.Dump.signal x
  | `Signaled x -> failf "%t failed with signal %a" pp_cmd Fmt.Dump.signal x

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

let unlink = Path.unlink

let rm_f_tree root =
  let rec rmtree path =
    let info =
      Path.with_open_in path File.stat
    in
    match info.File.Stat.kind with
    | `Directory ->
      (* Lwt_unix.chmod path 0o700 >>= fun () -> *)
      Path.read_dir path
      |> List.iter (function
          | "." | ".." -> ()
          | leaf -> rmtree (Path.(path / leaf))
        );
      Path.rmdir path
    | _ -> unlink path
  in
  rmtree root

let with_tmpdir ?prefix (fs : Eio.Fs.dir Eio.Path.t) (fn : Eio.Fs.dir Eio.Path.t -> 'a) =
  let tmpdir = make_tmp_dir ?prefix ~mode:0o700 (Filename.get_temp_dir_name ()) in
  let tmpdir = Eio.Path.(fs / tmpdir) in
  Fun.protect
    (fun () -> fn tmpdir)
    ~finally:(fun () -> rm_f_tree tmpdir)

let pp_command pp_cmd cmd f = Fmt.pf f "Command %a" pp_cmd cmd

let copy_to_log ~job src =
  let buf = Cstruct.create 4096 in
  let rec aux () =
    match Flow.single_read src buf with
    | 0 | exception End_of_file -> ()
    | n -> Job.write job (Cstruct.sub buf 0 n |> Cstruct.to_string); aux ()
  in
  aux ()

let sink_of_job_log job = object (_ : <Eio.Flow.sink;..>)
  method probe _ = None
  method copy src = copy_to_log ~job src
  method write buf = Job.write job (Cstruct.concat buf |> Cstruct.to_string)
end

let add_shutdown_hooks ~cancellable ~job ~cmd proc =
  if cancellable then (
    (* TODO: Don't think we need because switch will cancel for us *)
    Job.on_cancel job (fun reason ->
        Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
        Eio.Process.signal proc Sys.sigkill
        (* if proc#state = Lwt_process.Running then (
          Log.info (fun f -> f "Cancelling %a (%s)" pp_cmd cmd reason);
          proc#terminate;
        );
        Lwt.return_unit *)
      )
  )

let exec ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job proc cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let r, w = Eio.Process.pipe proc ~sw:job.switch in
  let child = Process.spawn ?cwd ~stdin:(Flow.string_source stdin) ~stdout:w ~stderr:w ~sw:job.switch proc (snd cmd) in
  add_shutdown_hooks ~cancellable ~job ~cmd child;
  Flow.close w;
  copy_to_log ~job r;
  Flow.close r;
  let status = Process.await child in
  match check_status pp_error_command cmd status with
  | Ok () -> Ok ()
  | Error _ as e -> e

let check_output ?cwd ?(stdin="") ?(pp_cmd = pp_cmd) ?pp_error_command ~cancellable ~job proc cmd =
  let pp_error_command = Option.value pp_error_command ~default:(pp_command pp_cmd cmd) in
  Log.info (fun f -> f "Exec: @[%a@]" pp_cmd cmd);
  Job.log job "Exec: @[%a@]" pp_cmd cmd;
  let stderr = sink_of_job_log job in
  let stdout_buffer = Buffer.create 128 in
  let stdout = Flow.buffer_sink stdout_buffer in
  let proc = Eio.Process.spawn ~sw:job.switch ~stdin:(Flow.string_source stdin) ~stdout ~stderr ?cwd proc (snd cmd) in
  add_shutdown_hooks ~cancellable ~job ~cmd proc;
  let status = Process.await proc in
  match check_status pp_error_command cmd status with
  | Error _ as e -> e
  | Ok () -> Ok (Buffer.contents stdout_buffer)