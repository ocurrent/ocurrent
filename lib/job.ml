(* For unit-tests: *)
let timestamp = ref Unix.gettimeofday
let sleep = ref Lwt_unix.sleep

type t = {
  switch : Switch.t;
  log : Fpath.t;
  set_start_time : float Lwt.u;
  start_time : float Lwt.t;
  mutable ch : out_channel option;
}

let open_temp_file ~dir ~prefix ~suffix =
  let path, ch = Filename.open_temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path, ch

let write t msg =
  match t.ch with
  | None -> Log.err (fun f -> f "Job.write(%a, %S) called on closed job" Fpath.pp t.log msg)
  | Some ch ->
    output_string ch msg;
    flush ch

let log t fmt =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    !timestamp () |> Unix.gmtime in
  let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
  Fmt.kstrf (write t) fmt
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let id t =
  match Fpath.split_base t.log with
  | parent_dir, leaf ->
    Fpath.(base parent_dir // leaf) |> Fpath.to_string

let fd t =
  match t.ch with
  | None -> Fmt.failwith "Job.fd(%a) called on closed job" Fpath.pp t.log
  | Some ch -> Unix.descr_of_out_channel ch

let jobs_dir = lazy (Disk_store.state_dir "job")

let log_path job_id =
  let open Astring in
  let jobs_dir = Lazy.force jobs_dir in
  match String.cuts ~sep:"/" job_id with
  | [date; file] when
      not (String.is_prefix ~affix:"." date) &&
      not (String.is_prefix ~affix:"." file) &&
      String.is_suffix ~affix:".log" file ->
    let path = Fpath.(jobs_dir / date / file) in
    begin match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Error (`Msg (Fmt.strf "Job log %a does not exist" Fpath.pp path))
      | Error _ as e -> e
    end
  | _ -> Error (`Msg (Fmt.strf "Invalid job ID %S" job_id))

let create ~switch ~label () =
  if not (Switch.is_on switch) then Fmt.failwith "Switch %a is not on! (%s)" Switch.pp switch label;
  let jobs_dir = Lazy.force jobs_dir in
  let time = !timestamp () |> Unix.gmtime in
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
    let start_time, set_start_time = Lwt.wait () in
    let t = { switch; log = path; ch = Some ch; start_time; set_start_time } in
    Switch.add_hook_or_fail switch (fun reason ->
        begin match reason with
          | Ok () -> ()
          | Error (`Msg m) ->
            log t "%s" m
        end;
        close_out ch;
        t.ch <- None;
        Lwt.return_unit
      );
    t

let pp_id = Fmt.string

let is_running t = Lwt.state t.start_time <> Lwt.Sleep

let start ?timeout t =
  if is_running t then (
    Log.warn (fun f -> f "start called, but job %a is already running!" Fpath.pp t.log);
    Fmt.failwith "Job.start called twice!"
  );
  Lwt.wakeup t.set_start_time (!timestamp ());
  Option.iter (Switch.add_timeout t.switch) timeout;
  Lwt.return_unit

let start_time t = t.start_time
