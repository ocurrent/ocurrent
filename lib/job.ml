type t = Fpath.t * out_channel

let open_temp_file ~dir ~prefix ~suffix =
  let path, ch = Filename.open_temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path, ch

let create ~switch ~label () =
  let jobs_dir = Disk_store.state_dir "job" in
  let time = Unix.gettimeofday () |> Unix.gmtime in
  let date =
    let { Unix.tm_year; tm_mon; tm_mday; _ } = time in
    Fmt.strf "%04d-%02d-%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
  in
  let date_dir = Fpath.(jobs_dir / date) in
  match Bos.OS.Dir.create date_dir with
  | Error (`Msg m) -> failwith m
  | Ok (_ : bool) ->
    let prefix =
      let { Unix.tm_hour; tm_min; tm_sec; _ } = time in
      Fmt.strf "%02d%02d%02d-%s-" tm_hour tm_min tm_sec label
    in
    let path, ch = open_temp_file ~dir:date_dir ~prefix ~suffix:".log" in
    Log.info (fun f -> f "Created new log file at@ %a" Fpath.pp path);
    Lwt_switch.add_hook (Some switch) (fun () -> close_out ch; Lwt.return_unit);
    path, ch

let write (_, ch) msg =
  output_string ch msg;
  flush ch

let log t fmt =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gettimeofday () |> Unix.gmtime in
  let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
  Fmt.kstrf (write t) fmt
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let id (path, _) =
  match Fpath.split_base path with
  | parent_dir, leaf ->
    Fpath.(base parent_dir // leaf) |> Fpath.to_string

let fd (_, ch) =
  Unix.descr_of_out_channel ch
