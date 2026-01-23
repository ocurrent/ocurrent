(* Unified RPC client implementation.

   This module provides:
   - All client operations (pipeline overview, job control, configuration)
   - Cmdliner integration for embedding in applications
   - Standalone client command
*)

open Lwt.Infix

(* ===== Helper Functions ===== *)

let pp_timestamp ppf ts =
  let open Unix in
  let tm = localtime ts in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let level_to_string = function
  | Engine.Harmless -> "harmless"
  | Engine.Mostly_harmless -> "mostly-harmless"
  | Engine.Average -> "average"
  | Engine.Above_average -> "above-average"
  | Engine.Dangerous -> "dangerous"

let string_to_level = function
  | "harmless" -> Some Engine.Harmless
  | "mostly-harmless" -> Some Engine.Mostly_harmless
  | "average" -> Some Engine.Average
  | "above-average" -> Some Engine.Above_average
  | "dangerous" -> Some Engine.Dangerous
  | "none" | "disabled" -> None
  | s -> Fmt.failwith "Unknown confirmation level: %S" s

(* ===== Client Operations ===== *)

module Ops = struct
  (* Pipeline overview *)
  let overview engine =
    Engine.pipeline_stats engine >>= function
    | Error `Capnp e ->
      Fmt.epr "Failed to get stats: %a@." Capnp_rpc.Error.pp e;
      Lwt_result.fail (`Capnp e)
    | Ok stats ->
      Fmt.pr "@[<v>Pipeline Statistics:@,\
              @,\
              OK:                       %d@,\
              Waiting for confirmation: %d@,\
              Ready:                    %d@,\
              Running:                  %d@,\
              Failed:                   %d@,\
              Blocked:                  %d@,\
              @,\
              Total stages: %d@]@.@."
        stats.ok
        stats.waiting_for_confirmation
        stats.ready
        stats.running
        stats.failed
        stats.blocked
        (stats.ok + stats.waiting_for_confirmation + stats.ready +
         stats.running + stats.failed + stats.blocked);
      Engine.pipeline_state engine >>= function
      | Error `Capnp e ->
        Fmt.epr "Failed to get state: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok state ->
        let state_str = match state with
          | Engine.Success -> "SUCCESS"
          | Engine.Failed msg -> Fmt.str "FAILED: %s" msg
          | Engine.Active `Ready -> "ACTIVE (ready to run)"
          | Engine.Active `Running -> "ACTIVE (running)"
          | Engine.Active `Waiting_for_confirmation -> "ACTIVE (waiting for confirmation)"
        in
        Fmt.pr "Pipeline State: %s@." state_str;
        Lwt_result.return ()

  (* List active jobs *)
  let jobs engine =
    Engine.active_jobs engine >>= function
    | Error `Capnp e ->
      Fmt.epr "Failed to list jobs: %a@." Capnp_rpc.Error.pp e;
      Lwt_result.fail (`Capnp e)
    | Ok jobs ->
      if jobs = [] then
        Fmt.pr "No active jobs.@."
      else begin
        Fmt.pr "@[<v>Active Jobs (%d):@," (List.length jobs);
        List.iter (fun j -> Fmt.pr "  %s@," j) jobs;
        Fmt.pr "@]@."
      end;
      Lwt_result.return ()

  (* Show job status *)
  let status engine job_id =
    let job = Engine.job engine job_id in
    Lwt.finalize
      (fun () ->
         Job.status job >>= function
         | Error `Capnp e ->
           Fmt.epr "Failed to get status: %a@." Capnp_rpc.Error.pp e;
           Lwt_result.fail (`Capnp e)
         | Ok { Job.id; description; can_cancel; can_rebuild } ->
           Fmt.pr "@[<v2>Job %S:@,\
                   Description: @[%a@]@,\
                   Can cancel: %b@,\
                   Can rebuild: %b@]@."
             id Fmt.lines description can_cancel can_rebuild;
           Lwt_result.return ())
      (fun () -> Capnp_rpc_lwt.Capability.dec_ref job; Lwt.return_unit)

  (* Show job log *)
  let log engine job_id =
    let job = Engine.job engine job_id in
    let rec stream start =
      Job.log ~start job >>= function
      | Error `Capnp e ->
        Fmt.epr "Failed to get log: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok (data, next) ->
        if data = "" then Lwt_result.return ()
        else begin
          output_string stdout data;
          flush stdout;
          stream next
        end
    in
    Lwt.finalize
      (fun () -> stream 0L)
      (fun () -> Capnp_rpc_lwt.Capability.dec_ref job; Lwt.return_unit)

  (* Cancel a job *)
  let cancel engine job_id =
    let job = Engine.job engine job_id in
    Lwt.finalize
      (fun () ->
         Job.cancel job >>= function
         | Error `Capnp e ->
           Fmt.epr "Failed to cancel: %a@." Capnp_rpc.Error.pp e;
           Lwt_result.fail (`Capnp e)
         | Ok () ->
           Fmt.pr "Cancelled.@.";
           Lwt_result.return ())
      (fun () -> Capnp_rpc_lwt.Capability.dec_ref job; Lwt.return_unit)

  (* Rebuild a single job *)
  let rebuild engine job_id =
    let job = Engine.job engine job_id in
    Fmt.pr "Requesting rebuild...@.";
    let new_job = Job.rebuild job in
    let rec stream start =
      Job.log ~start new_job >>= function
      | Error `Capnp e ->
        Fmt.epr "Failed to get log: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok (data, next) ->
        if data = "" then Lwt_result.return ()
        else begin
          output_string stdout data;
          flush stdout;
          stream next
        end
    in
    Lwt.finalize
      (fun () -> stream 0L)
      (fun () ->
         Capnp_rpc_lwt.Capability.dec_ref job;
         Capnp_rpc_lwt.Capability.dec_ref new_job;
         Lwt.return_unit)

  (* Approve early start *)
  let start engine job_id =
    let job = Engine.job engine job_id in
    Lwt.finalize
      (fun () ->
         Job.approve_early_start job >>= function
         | Error `Capnp e ->
           Fmt.epr "Failed to approve: %a@." Capnp_rpc.Error.pp e;
           Lwt_result.fail (`Capnp e)
         | Ok () ->
           Fmt.pr "Job approved to start.@.";
           Lwt_result.return ())
      (fun () -> Capnp_rpc_lwt.Capability.dec_ref job; Lwt.return_unit)

  (* Query job history *)
  let query engine ~op ~ok ~rebuild ~job_prefix =
    let params = { Engine.op; ok; rebuild; job_prefix } in
    Engine.query engine params >>= function
    | Error `Capnp e ->
      Fmt.epr "Query failed: %a@." Capnp_rpc.Error.pp e;
      Lwt_result.fail (`Capnp e)
    | Ok entries ->
      if entries = [] then
        Fmt.pr "No matching jobs found.@."
      else begin
        Fmt.pr "@[<v>Job History (%d entries):@,@," (List.length entries);
        entries |> List.iter (fun (entry : Engine.history_entry) ->
          let outcome_str = match entry.outcome with
            | Ok v when v = "" -> "OK"
            | Ok v -> Fmt.str "OK: %s" v
            | Error e -> Fmt.str "FAILED: %s" e
          in
          let running_str = match entry.running with
            | Some t -> Fmt.str "  Started:  %a@," pp_timestamp t
            | None -> ""
          in
          Fmt.pr "@[<v>Job: %s (build #%Ld)@,\
                  Outcome: %s@,\
                  Ready:    %a@,\
                  %s\
                  Finished: %a@,\
                  Rebuild requested: %b@]@,@,"
            entry.job_id entry.build
            outcome_str
            pp_timestamp entry.ready
            running_str
            pp_timestamp entry.finished
            entry.rebuild
        );
        Fmt.pr "@]@."
      end;
      Lwt_result.return ()

  (* List operation types *)
  let ops engine =
    Engine.ops engine >>= function
    | Error `Capnp e ->
      Fmt.epr "Failed to list ops: %a@." Capnp_rpc.Error.pp e;
      Lwt_result.fail (`Capnp e)
    | Ok ops ->
      if ops = [] then
        Fmt.pr "No operation types found.@."
      else begin
        Fmt.pr "@[<v>Operation Types:@,";
        List.iter (fun op -> Fmt.pr "  %s@," op) ops;
        Fmt.pr "@]@."
      end;
      Lwt_result.return ()

  (* Get pipeline DOT graph *)
  let dot engine =
    Engine.pipeline_dot engine >>= function
    | Error `Capnp e ->
      Fmt.epr "Failed to get DOT: %a@." Capnp_rpc.Error.pp e;
      Lwt_result.fail (`Capnp e)
    | Ok dot ->
      print_string dot;
      Lwt_result.return ()

  (* Get/set confirmation level *)
  let confirm engine set_level =
    match set_level with
    | Some level_str ->
      let level = string_to_level level_str in
      Engine.set_confirm_level engine level >>= begin function
      | Error `Capnp e ->
        Fmt.epr "Failed to set level: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok () ->
        (match level with
         | None -> Fmt.pr "Confirmation disabled.@."
         | Some l -> Fmt.pr "Confirmation level set to: %s@." (level_to_string l));
        Lwt_result.return ()
      end
    | None ->
      Engine.get_confirm_level engine >>= function
      | Error `Capnp e ->
        Fmt.epr "Failed to get level: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok level ->
        (match level with
         | None -> Fmt.pr "Confirmation: disabled@."
         | Some l -> Fmt.pr "Confirmation level: %s@." (level_to_string l));
        Lwt_result.return ()

  (* Bulk rebuild *)
  let rebuild_all engine job_ids =
    if job_ids = [] then begin
      Fmt.pr "No job IDs specified.@.";
      Lwt_result.return ()
    end else begin
      Fmt.pr "Requesting rebuild of %d jobs...@." (List.length job_ids);
      Engine.rebuild_all engine job_ids >>= function
      | Error `Capnp e ->
        Fmt.epr "Rebuild failed: %a@." Capnp_rpc.Error.pp e;
        Lwt_result.fail (`Capnp e)
      | Ok result ->
        if result.succeeded <> [] then begin
          Fmt.pr "@[<v>Successfully queued for rebuild:@,";
          List.iter (fun id -> Fmt.pr "  %s@," id) result.succeeded;
          Fmt.pr "@]"
        end;
        if result.failed <> [] then begin
          Fmt.pr "@[<v>Failed to rebuild:@,";
          List.iter (fun id -> Fmt.pr "  %s@," id) result.failed;
          Fmt.pr "@]"
        end;
        Fmt.pr "@.";
        Lwt_result.return ()
    end
end

(* ===== Connection Handling ===== *)

let connect cap_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Vat.import_exn vat cap_uri in
  Capnp_rpc_lwt.Sturdy_ref.connect_exn sr

let with_engine cap_uri f =
  connect cap_uri >>= fun engine ->
  Lwt.finalize
    (fun () -> f engine)
    (fun () -> Capnp_rpc_lwt.Capability.dec_ref engine; Lwt.return_unit)

(* ===== Cmdliner Integration ===== *)

module Cmdliner = struct
  open Cmdliner

  (* Build subcommands parameterized by cap_uri term *)
  let make_subcommands cap_uri =
    (* Subcommand: overview *)
    let overview_cmd =
      let doc = "Show pipeline statistics and state" in
      let run cap_uri =
        match Lwt_main.run (with_engine cap_uri Ops.overview) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "overview" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: jobs *)
    let jobs_cmd =
      let doc = "List active jobs" in
      let run cap_uri =
        match Lwt_main.run (with_engine cap_uri Ops.jobs) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "jobs" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: status *)
    let status_cmd =
      let doc = "Show status of a specific job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.status e job_id)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "status" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: log *)
    let log_cmd =
      let doc = "Show log of a specific job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.log e job_id)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "log" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: cancel *)
    let cancel_cmd =
      let doc = "Cancel a running job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.cancel e job_id)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "cancel" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: rebuild *)
    let rebuild_cmd =
      let doc = "Rebuild a job" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.rebuild e job_id)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "rebuild" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: start *)
    let start_cmd =
      let doc = "Approve early start for a job waiting for confirmation" in
      let job_id =
        Arg.required @@
        Arg.pos 0 Arg.(some string) None @@
        Arg.info [] ~doc:"The job ID" ~docv:"JOB_ID"
      in
      let run cap_uri job_id =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.start e job_id)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "start" ~doc) Term.(ret (const run $ cap_uri $ job_id))
    in

    (* Subcommand: query *)
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
      let prefix =
        Arg.value @@
        Arg.opt Arg.(some string) None @@
        Arg.info ["prefix"] ~doc:"Filter by job ID prefix (e.g., date)" ~docv:"PREFIX"
      in
      let run cap_uri op ok rebuild job_prefix =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.query e ~op ~ok ~rebuild ~job_prefix)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "query" ~doc) Term.(ret (const run $ cap_uri $ op $ ok $ rebuild $ prefix))
    in

    (* Subcommand: ops *)
    let ops_cmd =
      let doc = "List operation types" in
      let run cap_uri =
        match Lwt_main.run (with_engine cap_uri Ops.ops) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "ops" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: dot *)
    let dot_cmd =
      let doc = "Output pipeline as DOT graph (pipe to 'dot -Tsvg' for visualization)" in
      let run cap_uri =
        match Lwt_main.run (with_engine cap_uri Ops.dot) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "dot" ~doc) Term.(ret (const run $ cap_uri))
    in

    (* Subcommand: confirm *)
    let confirm_cmd =
      let doc = "Get or set confirmation level" in
      let set_level =
        Arg.value @@
        Arg.opt Arg.(some string) None @@
        Arg.info ["set"]
          ~doc:"Set level (harmless, mostly-harmless, average, above-average, dangerous, none)"
          ~docv:"LEVEL"
      in
      let run cap_uri set_level =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.confirm e set_level)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "confirm" ~doc) Term.(ret (const run $ cap_uri $ set_level))
    in

    (* Subcommand: rebuild-all *)
    let rebuild_all_cmd =
      let doc = "Rebuild multiple jobs" in
      let job_ids =
        Arg.value @@
        Arg.pos_all Arg.string [] @@
        Arg.info [] ~doc:"Job IDs to rebuild" ~docv:"JOB_ID"
      in
      let run cap_uri job_ids =
        match Lwt_main.run (with_engine cap_uri (fun e -> Ops.rebuild_all e job_ids)) with
        | Ok () -> `Ok ()
        | Error `Capnp e -> `Error (false, Fmt.str "%a" Capnp_rpc.Error.pp e)
      in
      Cmd.v (Cmd.info "rebuild-all" ~doc) Term.(ret (const run $ cap_uri $ job_ids))
    in

    [
      overview_cmd;
      jobs_cmd;
      status_cmd;
      log_cmd;
      cancel_cmd;
      rebuild_cmd;
      start_cmd;
      query_cmd;
      ops_cmd;
      dot_cmd;
      confirm_cmd;
      rebuild_all_cmd;
    ]

  (* Common arguments - required cap_uri for standalone *)
  let cap_uri =
    Arg.required @@
    Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
    Arg.info ["connect"; "c"]
      ~doc:"Path to the capability file or capability URI."
      ~docv:"ADDR"

  (* Cap_uri with a default value for embedded use *)
  let cap_uri_with_default default =
    Arg.value @@
    Arg.opt Capnp_rpc_unix.sturdy_uri default @@
    Arg.info ["connect"; "c"]
      ~doc:"Path to the capability file or capability URI."
      ~docv:"ADDR"

  (* Subcommands using required cap_uri *)
  let subcommands = make_subcommands cap_uri

  let overview_cmd = List.nth subcommands 0
  let jobs_cmd = List.nth subcommands 1
  let status_cmd = List.nth subcommands 2
  let log_cmd = List.nth subcommands 3
  let cancel_cmd = List.nth subcommands 4
  let rebuild_cmd = List.nth subcommands 5
  let start_cmd = List.nth subcommands 6
  let query_cmd = List.nth subcommands 7
  let ops_cmd = List.nth subcommands 8
  let dot_cmd = List.nth subcommands 9
  let confirm_cmd = List.nth subcommands 10
  let rebuild_all_cmd = List.nth subcommands 11

  (* Client command group for embedding in applications *)
  let client_cmd ?(name="client") ?cap_file () =
    let doc = "RPC client commands for querying and controlling the pipeline" in
    let man = [
      `S Manpage.s_description;
      `P "Commands for interacting with a running pipeline via Cap'n Proto RPC.";
      `S Manpage.s_commands;
      `I ("overview", "Show pipeline statistics and state");
      `I ("jobs", "List active jobs");
      `I ("status JOB_ID", "Show job status");
      `I ("log JOB_ID", "Stream job log");
      `I ("cancel JOB_ID", "Cancel a running job");
      `I ("rebuild JOB_ID", "Rebuild a job");
      `I ("start JOB_ID", "Approve early start");
      `I ("query", "Query job history");
      `I ("ops", "List operation types");
      `I ("dot", "Output pipeline graph in DOT format");
      `I ("confirm", "Get/set confirmation level");
      `I ("rebuild-all", "Rebuild multiple jobs");
    ] in
    let cmds = match cap_file with
      | Some f ->
        let default_uri = Uri.of_string f in
        make_subcommands (cap_uri_with_default default_uri)
      | None -> subcommands
    in
    let info = Cmd.info name ~doc ~man in
    Cmd.group ~default:Term.(ret (const (`Help (`Pager, None)))) info cmds

  (* Standalone client command *)
  let standalone_cmd ?(name="ocurrent-rpc") () =
    let doc = "OCurrent RPC client" in
    let man = [
      `S Manpage.s_description;
      `P "A command-line client for interacting with OCurrent pipelines via RPC.";
      `P "Use --connect/-c to specify the capability file for the pipeline you want to connect to.";
      `S Manpage.s_examples;
      `P "Show pipeline overview:";
      `Pre "  $(mname) --connect=./engine.cap overview";
      `P "List active jobs:";
      `Pre "  $(mname) -c ./engine.cap jobs";
      `P "Get job log:";
      `Pre "  $(mname) -c ./engine.cap log 2024-01-15/123456-docker-build-abc123";
      `P "Generate SVG visualization:";
      `Pre "  $(mname) -c ./engine.cap dot | dot -Tsvg > pipeline.svg";
    ] in
    let info = Cmd.info name ~doc ~man in
    Cmd.group ~default:Term.(ret (const (`Help (`Pager, None)))) info subcommands
end
