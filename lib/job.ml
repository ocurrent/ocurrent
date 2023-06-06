module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "core"

  let active_jobs =
    let help = "Number of ready or running job" in
    Gauge.v ~help ~namespace ~subsystem "active_jobs"
end

module Map = Map.Make(String)

(* For unit-tests: *)
let timestamp = ref Unix.gettimeofday
let sleep = ref Eio_unix.sleep

type t = {
  switch : Eio.Switch.t;
  config : Config.t;
  id : string;
  priority : Pool.priority;
  set_start_time : float Eio.Promise.u;
  start_time : float Eio.Promise.t;
  mutable path : Fpath.t option;
  log_cond : Eio.Condition.t;  (* Fires whenever log data is written, or log is closed. *)
  explicit_confirm : unit Eio.Promise.or_exn;
  set_explicit_confirm : (unit, exn) result Eio.Promise.u; (* Resolve this to override the global confirmation threshold. *)
  mutable cancel_hooks : [ `Hooks of (string -> unit) Lwt_dllist.t | `Cancelled of string ];
  mutable waiting_for_confirmation : bool;  (* Is calling [approve_early_start] useful? *)
}

let jobs = ref Map.empty

let temp_file ~dir ~prefix ~suffix =
  let path = Filename.temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path

let write t msg =
  match t.path with
  | None -> Log.err (fun f -> f "Job.write(%s, %S) called on closed job" t.id msg)
  | Some path ->
    let ch = open_out_gen [Open_wronly; Open_append] 0o600 (Fpath.to_string path) in
    Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
      output_string ch msg;
      flush ch;
    );
    Eio.Condition.broadcast t.log_cond

let log t fmt =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    !timestamp () |> Unix.gmtime in
  let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
  Fmt.kstr (write t) fmt
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let id t = t.id

let jobs_dir = lazy (Disk_store.state_dir "job")

let log_path job_id =
  let open Astring in
  let jobs_dir = Lazy.force jobs_dir in
  match String.cuts ~sep:"/" job_id with
  | [date; file] when
      not (String.is_prefix ~affix:"." date) &&
      not (String.is_prefix ~affix:"." file) ->
    let path = Fpath.(jobs_dir / date / (file ^ ".log")) in
    begin match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Error (`Msg (Fmt.str "Job log %a does not exist" Fpath.pp path))
      | Error _ as e -> e
    end
  | _ -> Error (`Msg (Fmt.str "Invalid job ID %S" job_id))

let id_of_path path =
  match Fpath.split_base path with
  | parent_dir, leaf ->
    Fpath.(base parent_dir // leaf |> segs) |> String.concat "/" |> Filename.chop_extension

let run_cancel_hooks ~reason hooks =
  let rec aux () =
    match Lwt_dllist.take_opt_l hooks with
    | None -> ()
    | Some fn -> fn reason; aux ()
  in
  aux ()

let cancel t reason =
  match t.cancel_hooks with
  | `Cancelled r2 ->
    log t "cancel(%S): already cancelled (%S)!" reason r2
  | `Hooks hooks ->
    t.cancel_hooks <- `Cancelled reason;
    log t "Cancelling: %s" reason;
    Eio.Fiber.fork ~sw:t.switch (fun () ->
        try run_cancel_hooks ~reason hooks with
          | Unix.Unix_error(Unix.EPERM, "kill", _) ->
            log t "cancel(%S, %S): permission denied when killing child process (job has used sudo?)" (id t) reason
          | ex ->
            Fmt.failwith "Uncaught exception from cancel hook for %S: %a" (id t) Fmt.exn ex
        )


let create ?(priority=`Low) ~sw ~label ~config () =
  (* if not (Eio.Switch. switch) then Fmt.failwith "Switch %a is not on! (%s)" Switch.pp switch label; *)
  let jobs_dir = Lazy.force jobs_dir in
  let time = !timestamp () |> Unix.gmtime in
  let date =
    let { Unix.tm_year; tm_mon; tm_mday; _ } = time in
    Fmt.str "%04d-%02d-%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
  in
  let date_dir = Fpath.(jobs_dir / date) in
  match Bos.OS.Dir.create date_dir with
  | Error (`Msg m) -> failwith m
  | Ok (_ : bool) ->
    let prefix =
      let { Unix.tm_hour; tm_min; tm_sec; _ } = time in
      Fmt.str "%02d%02d%02d-%s-" tm_hour tm_min tm_sec label
    in
    let path = temp_file ~dir:date_dir ~prefix ~suffix:".log" in
    Log.info (fun f -> f "Created new log file at@ %a" Fpath.pp path);
    let id = id_of_path path in
    let start_time, set_start_time = Eio.Promise.create () in
    let log_cond = Eio.Condition.create () in
    let explicit_confirm, set_explicit_confirm = Eio.Promise.create () in
    let cancel_hooks = `Hooks (Lwt_dllist.create ()) in
    let t = { switch=sw; id; path = Some path; start_time; set_start_time; config; log_cond; cancel_hooks;
              explicit_confirm; set_explicit_confirm; waiting_for_confirmation = false; priority } in
    jobs := Map.add id t !jobs;
    Prometheus.Gauge.inc_one Metrics.active_jobs;
    Eio.Switch.on_release sw (fun () ->
        begin match t.cancel_hooks with
          | `Hooks hooks ->
            let reason = "Job complete" in
            t.cancel_hooks <- `Cancelled reason;
            run_cancel_hooks ~reason hooks
          | `Cancelled _ -> ()
        end;
        t.path <- None;
        jobs := Map.remove id !jobs;
        Prometheus.Gauge.dec_one Metrics.active_jobs;
        Eio.Condition.broadcast t.log_cond
      );
    t

let pp_id = Fmt.string

let is_running t = Eio.Promise.is_resolved t.start_time

let on_cancel t fn =
  match t.cancel_hooks with
  | `Cancelled reason -> fn reason
  | `Hooks hooks ->
    let (_ : _ Lwt_dllist.node) = Lwt_dllist.add_r fn hooks in
    ()

let with_handler t ~on_cancel fn =
  match t.cancel_hooks with
  | `Cancelled reason ->
    on_cancel reason |> fn
  | `Hooks hooks ->
    let node = Lwt_dllist.add_r on_cancel hooks in
    Fun.protect fn ~finally:(fun () -> Lwt_dllist.remove node)

let use_pool ?(priority=`Low) t pool =
  let th, cancel = Pool.get ~sw:t.switch ~priority pool in
  on_cancel t (fun _ -> cancel ());
  th

let no_pool =
  Pool.of_fn ~label:"no pool" (fun ~sw:_ ~priority:_ -> Eio.Promise.create_resolved (Ok ()), fun () -> ())

let confirm t ~pool level =
  let confirmed () =
    let confirmed, set_confirmed = Eio.Promise.create () in
    Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork ~sw (fun () -> Config.confirmed level t.config; Eio.Promise.resolve_ok set_confirmed ());
    on_cancel t (fun _ -> if not (Eio.Promise.is_resolved confirmed) then Eio.Promise.resolve_error set_confirmed (Failure "Cancelled"));
    match Eio.Promise.is_resolved confirmed with
    | true -> ()
    | false ->
      log t "Waiting for confirm-threshold > %a" Level.pp level;
      Log.info (fun f -> f "Waiting for confirm-threshold > %a" Level.pp level);
      t.waiting_for_confirmation <- true;
      Eio.Fiber.any [
        (fun () -> Eio.Promise.await_exn confirmed);
        (fun () -> Eio.Promise.await_exn t.explicit_confirm)
      ];
      t.waiting_for_confirmation <- false;
      if Eio.Promise.is_resolved confirmed then (
        log t "Confirm-threshold now > %a" Level.pp level;
        Log.info (fun f -> f "Confirm-threshold now > %a" Level.pp level)
      );
      if Eio.Promise.is_resolved t.explicit_confirm then (
        log t "Explicit approval received for this job"
      )
  in
  confirmed ();
  let res = use_pool t ~priority:t.priority pool in
  if not (Eio.Promise.is_resolved res) then (
    log t "Waiting for resource in pool %a" Pool.pp pool;
    let r = Eio.Promise.await res in
    log t "Got resource from pool %a" Pool.pp pool;
    Eio.Promise.create_resolved r
  ) else res

let pp_duration f d =
  let d = Duration.to_f d in
  if d > 120.0 then Fmt.pf f "%.1f minutes" (d /. 60.)
  else if d > 2.0 then Fmt.pf f "%.1f seconds" d
  else Fmt.pf f "%f seconds" d

let start_with ?timeout ~pool ~level t =
  let r = Eio.Promise.await_exn @@ confirm t ~pool level in
  if is_running t then (
    Log.warn (fun f -> f "start called, but job %s is already running!" t.id);
    Fmt.failwith "Job.start called twice!"
  );
  Eio.Promise.resolve t.set_start_time (!timestamp ());
  timeout |> Option.iter (fun duration ->
      (* We could be smarter about this and cancel the timeout when the switch is turned off. *)
    Eio.Fiber.fork ~sw:t.switch (fun () ->
      Eio_unix.sleep (Duration.to_f duration);
      match t.cancel_hooks with
      | `Cancelled _ -> ()
      | `Hooks _ -> cancel t (Fmt.str "Timeout (%a)" pp_duration duration)
    )
  );
  r

let start ?timeout ?(pool=no_pool) = start_with ?timeout ~pool

let start_time t = t.start_time

let wait_for_log_data t = Eio.Condition.await_no_mutex t.log_cond

let lookup_running id = Map.find_opt id !jobs

let is_waiting_for_confirmation t = t.waiting_for_confirmation

let approve_early_start t =
  match Eio.Promise.peek t.explicit_confirm with
  | None -> Eio.Promise.resolve_ok t.set_explicit_confirm ()
  | Some (Ok ()) -> ()
  | Some (Error ex) -> raise ex

let cancelled_state t =
  match t.cancel_hooks with
  | `Cancelled reason -> Error (`Msg reason)
  | `Hooks _ -> Ok ()

let jobs () = !jobs
