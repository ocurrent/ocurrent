(** ocurrent-ctl - CLI tool for controlling OCurrent pipelines via RPC *)

open Lwt.Infix
open Capnp_rpc_lwt

let () = Prometheus_unix.Logging.init ~default_level:Logs.Warning ()

(* Convert RPC errors to simple messages for Cmdliner *)
let to_msg_error = function
  | Ok x -> Ok x
  | Error `Capnp ex -> Error (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp ex))

(* Helper for formatting timestamps *)
let pp_timestamp ppf ts =
  let open Unix in
  let tm = localtime ts in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(* Connect to engine *)
let connect cap_file =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Vat.import_exn vat cap_file in
  Sturdy_ref.connect_exn sr

(* ===== Engine Operations ===== *)

let list_jobs engine =
  Current_rpc.Engine.active_jobs engine |> Lwt_result.map @@ fun jobs ->
  if jobs = [] then
    Fmt.pr "No active jobs@."
  else begin
    Fmt.pr "@[<v>Active jobs:@,";
    List.iter (fun j -> Fmt.pr "  %s@," j) jobs;
    Fmt.pr "@]@."
  end

let query_jobs engine ~op ~ok ~rebuild ~job_prefix =
  let params = { Current_rpc.Engine.op; ok; rebuild; job_prefix } in
  Current_rpc.Engine.query engine params |> Lwt_result.map @@ fun entries ->
  if entries = [] then
    Fmt.pr "No matching jobs found@."
  else begin
    Fmt.pr "@[<v>Job History:@,@,";
    entries |> List.iter (fun (entry : Current_rpc.Engine.history_entry) ->
      let outcome_str = match entry.outcome with
        | Ok v -> Fmt.str "OK: %s" v
        | Error e -> Fmt.str "FAILED: %s" e
      in
      let running_str = match entry.running with
        | Some t -> Fmt.str " started=%a" pp_timestamp t
        | None -> ""
      in
      Fmt.pr "  @[<v>%s (build %Ld)@,\
              outcome: %s@,\
              ready=%a%s finished=%a@,\
              rebuild=%b@]@,@,"
        entry.job_id entry.build
        outcome_str
        pp_timestamp entry.ready
        running_str
        pp_timestamp entry.finished
        entry.rebuild
    );
    Fmt.pr "@]@."
  end

let list_ops engine =
  Current_rpc.Engine.ops engine |> Lwt_result.map @@ fun ops ->
  if ops = [] then
    Fmt.pr "No operations found@."
  else begin
    Fmt.pr "@[<v>Operation types:@,";
    List.iter (fun op -> Fmt.pr "  %s@," op) ops;
    Fmt.pr "@]@."
  end

let show_stats engine =
  Current_rpc.Engine.pipeline_stats engine |> Lwt_result.map @@
  fun (stats : Current_rpc.Engine.stats) ->
  Fmt.pr "@[<v>Pipeline Statistics:@,\
          OK:                     %d@,\
          Waiting for confirmation: %d@,\
          Ready:                  %d@,\
          Running:                %d@,\
          Failed:                 %d@,\
          Blocked:                %d@]@."
    stats.ok
    stats.waiting_for_confirmation
    stats.ready
    stats.running
    stats.failed
    stats.blocked

let show_state engine =
  Current_rpc.Engine.pipeline_state engine |> Lwt_result.map @@ fun state ->
  let state_str = match state with
    | Current_rpc.Engine.Success -> "SUCCESS"
    | Current_rpc.Engine.Failed msg -> Fmt.str "FAILED: %s" msg
    | Current_rpc.Engine.Active `Ready -> "ACTIVE (ready)"
    | Current_rpc.Engine.Active `Running -> "ACTIVE (running)"
    | Current_rpc.Engine.Active `Waiting_for_confirmation -> "ACTIVE (waiting for confirmation)"
  in
  Fmt.pr "Pipeline State: %s@." state_str

let show_dot engine =
  Current_rpc.Engine.pipeline_dot engine |> Lwt_result.map @@ fun dot ->
  print_string dot

let get_confirm_level engine =
  Current_rpc.Engine.get_confirm_level engine |> Lwt_result.map @@ fun level ->
  match level with
  | None -> Fmt.pr "Confirmation: disabled@."
  | Some l ->
    let name = match l with
      | Current_rpc.Engine.Harmless -> "harmless"
      | Current_rpc.Engine.Mostly_harmless -> "mostly-harmless"
      | Current_rpc.Engine.Average -> "average"
      | Current_rpc.Engine.Above_average -> "above-average"
      | Current_rpc.Engine.Dangerous -> "dangerous"
    in
    Fmt.pr "Confirmation level: %s@." name

let set_confirm_level engine level =
  Current_rpc.Engine.set_confirm_level engine level |> Lwt_result.map @@ fun () ->
  match level with
  | None -> Fmt.pr "Confirmation disabled@."
  | Some l ->
    let name = match l with
      | Current_rpc.Engine.Harmless -> "harmless"
      | Current_rpc.Engine.Mostly_harmless -> "mostly-harmless"
      | Current_rpc.Engine.Average -> "average"
      | Current_rpc.Engine.Above_average -> "above-average"
      | Current_rpc.Engine.Dangerous -> "dangerous"
    in
    Fmt.pr "Confirmation level set to: %s@." name

let rebuild_all engine job_ids =
  Current_rpc.Engine.rebuild_all engine job_ids |> Lwt_result.map @@
  fun (result : Current_rpc.Engine.rebuild_result) ->
  if result.succeeded <> [] then begin
    Fmt.pr "@[<v>Rebuild queued:@,";
    List.iter (fun id -> Fmt.pr "  %s@," id) result.succeeded;
    Fmt.pr "@]"
  end;
  if result.failed <> [] then begin
    Fmt.pr "@[<v>Rebuild failed:@,";
    List.iter (fun id -> Fmt.pr "  %s@," id) result.failed;
    Fmt.pr "@]"
  end;
  Fmt.pr "@."

(* ===== Job Operations ===== *)

let show_log job =
  let rec aux start =
    Current_rpc.Job.log ~start job >>= function
    | Error _ as e -> Lwt.return e
    | Ok (data, next) ->
      if data = "" then Lwt_result.return ()
      else (
        output_string stdout data;
        flush stdout;
        aux next
      )
  in
  aux 0L

let show_status job =
  Current_rpc.Job.status job |> Lwt_result.map @@
  fun { Current_rpc.Job.id; description; can_cancel; can_rebuild } ->
  Fmt.pr "@[<v2>Job %S:@,\
          Description: @[%a@]@,\
          Can cancel: %b@,\
          Can rebuild: %b@]@."
    id
    Fmt.lines description
    can_cancel
    can_rebuild

let cancel job =
  Current_rpc.Job.cancel job |> Lwt_result.map @@ fun () ->
  Fmt.pr "Cancelled@."

let approve_start job =
  Current_rpc.Job.approve_early_start job >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    Fmt.pr "Job approved to start@.";
    Lwt_result.return ()

let rebuild_job job =
  Fmt.pr "Requesting rebuild...@.";
  let new_job = Current_rpc.Job.rebuild job in
  Capability.when_released new_job (fun () ->
    Fmt.pr "New job capability released@."
  );
  show_log new_job

(* ===== Command-line interface ===== *)

open Cmdliner

let cap_file =
  Arg.required @@
  Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info ["c"; "cap"] ~doc:"Path to the engine.cap file" ~docv:"CAP"

(* Engine subcommands *)

let jobs_cmd =
  let doc = "List active jobs" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> list_jobs engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "jobs" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let query_cmd =
  let doc = "Query job history" in
  let op =
    Arg.value @@
    Arg.opt Arg.(some string) None @@
    Arg.info ["op"] ~doc:"Filter by operation type" ~docv:"OP"
  in
  let ok =
    Arg.value @@
    Arg.opt Arg.(some bool) None @@
    Arg.info ["ok"] ~doc:"Filter by success (true) or failure (false)"
  in
  let rebuild =
    Arg.value @@
    Arg.opt Arg.(some bool) None @@
    Arg.info ["rebuild"] ~doc:"Filter by rebuild-needed flag"
  in
  let job_prefix =
    Arg.value @@
    Arg.opt Arg.(some string) None @@
    Arg.info ["prefix"] ~doc:"Filter by job ID prefix (e.g., date)" ~docv:"PREFIX"
  in
  let run cap_file op ok rebuild job_prefix =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> query_jobs engine ~op ~ok ~rebuild ~job_prefix)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "query" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ op $ ok $ rebuild $ job_prefix))

let ops_cmd =
  let doc = "List operation types" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> list_ops engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "ops" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let stats_cmd =
  let doc = "Show pipeline statistics" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> show_stats engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "stats" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let state_cmd =
  let doc = "Show pipeline state" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> show_state engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "state" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let dot_cmd =
  let doc = "Output pipeline as DOT graph" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> show_dot engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "dot" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let confirm_get_cmd =
  let doc = "Get confirmation level" in
  let run cap_file =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> get_confirm_level engine)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "confirm-get" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file))

let confirm_set_cmd =
  let doc = "Set confirmation level" in
  let level_conv =
    let parse s = match String.lowercase_ascii s with
      | "none" | "disabled" -> Ok None
      | "harmless" -> Ok (Some Current_rpc.Engine.Harmless)
      | "mostly-harmless" -> Ok (Some Current_rpc.Engine.Mostly_harmless)
      | "average" -> Ok (Some Current_rpc.Engine.Average)
      | "above-average" -> Ok (Some Current_rpc.Engine.Above_average)
      | "dangerous" -> Ok (Some Current_rpc.Engine.Dangerous)
      | _ -> Error (`Msg "Unknown level")
    in
    let print ppf = function
      | None -> Fmt.string ppf "none"
      | Some Current_rpc.Engine.Harmless -> Fmt.string ppf "harmless"
      | Some Current_rpc.Engine.Mostly_harmless -> Fmt.string ppf "mostly-harmless"
      | Some Current_rpc.Engine.Average -> Fmt.string ppf "average"
      | Some Current_rpc.Engine.Above_average -> Fmt.string ppf "above-average"
      | Some Current_rpc.Engine.Dangerous -> Fmt.string ppf "dangerous"
    in
    Arg.conv (parse, print)
  in
  let level =
    Arg.required @@
    Arg.pos 0 Arg.(some level_conv) None @@
    Arg.info [] ~doc:"Confirmation level (none, harmless, mostly-harmless, average, above-average, dangerous)" ~docv:"LEVEL"
  in
  let run cap_file level =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> set_confirm_level engine level)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "confirm-set" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ level))

let rebuild_all_cmd =
  let doc = "Rebuild multiple jobs" in
  let job_ids =
    Arg.non_empty @@
    Arg.pos_all Arg.string [] @@
    Arg.info [] ~doc:"Job IDs to rebuild" ~docv:"JOB_ID"
  in
  let run cap_file job_ids =
    Lwt_main.run begin
      connect cap_file >>= fun engine ->
      Lwt.finalize
        (fun () -> rebuild_all engine job_ids)
        (fun () -> Capability.dec_ref engine; Lwt.return_unit)
    end |> to_msg_error
  in
  let info = Cmd.info "rebuild-all" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_ids))

(* Job subcommands *)

let job_id =
  Arg.required @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info [] ~doc:"Job ID" ~docv:"JOB_ID"

let with_job cap_file job_id f =
  Lwt_main.run begin
    connect cap_file >>= fun engine ->
    let job = Current_rpc.Engine.job engine job_id in
    Lwt.finalize
      (fun () ->
        Lwt.finalize (fun () -> f job)
          (fun () -> Capability.dec_ref job; Lwt.return_unit))
      (fun () -> Capability.dec_ref engine; Lwt.return_unit)
  end |> to_msg_error

let job_status_cmd =
  let doc = "Show job status" in
  let run cap_file job_id = with_job cap_file job_id show_status in
  let info = Cmd.info "job-status" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_id))

let job_log_cmd =
  let doc = "Show job log" in
  let run cap_file job_id = with_job cap_file job_id show_log in
  let info = Cmd.info "job-log" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_id))

let job_cancel_cmd =
  let doc = "Cancel a job" in
  let run cap_file job_id = with_job cap_file job_id cancel in
  let info = Cmd.info "job-cancel" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_id))

let job_rebuild_cmd =
  let doc = "Rebuild a job" in
  let run cap_file job_id = with_job cap_file job_id rebuild_job in
  let info = Cmd.info "job-rebuild" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_id))

let job_approve_cmd =
  let doc = "Approve job to start early" in
  let run cap_file job_id = with_job cap_file job_id approve_start in
  let info = Cmd.info "job-approve" ~doc in
  Cmd.v info Term.(term_result (const run $ cap_file $ job_id))

(* Main command group *)

let main_cmd =
  let doc = "CLI tool for controlling OCurrent pipelines via RPC" in
  let man = [
    `S Manpage.s_description;
    `P "ocurrent-ctl connects to an OCurrent pipeline via Cap'n Proto RPC \
        and allows you to query and control the pipeline.";
    `S Manpage.s_commands;
    `P "Engine commands:";
    `I ("jobs", "List active jobs");
    `I ("query", "Query job history database");
    `I ("ops", "List operation types");
    `I ("stats", "Show pipeline statistics");
    `I ("state", "Show overall pipeline state");
    `I ("dot", "Output pipeline as DOT graph");
    `I ("confirm-get", "Get confirmation level");
    `I ("confirm-set", "Set confirmation level");
    `I ("rebuild-all", "Rebuild multiple jobs");
    `P "Job commands:";
    `I ("job-status", "Show job status");
    `I ("job-log", "Show job log");
    `I ("job-cancel", "Cancel a job");
    `I ("job-rebuild", "Rebuild a job");
    `I ("job-approve", "Approve job to start early");
  ] in
  let info = Cmd.info "ocurrent-ctl" ~doc ~man in
  Cmd.group info [
    jobs_cmd;
    query_cmd;
    ops_cmd;
    stats_cmd;
    state_cmd;
    dot_cmd;
    confirm_get_cmd;
    confirm_set_cmd;
    rebuild_all_cmd;
    job_status_cmd;
    job_log_cmd;
    job_cancel_cmd;
    job_rebuild_cmd;
    job_approve_cmd;
  ]

let () = exit @@ Cmd.eval main_cmd
