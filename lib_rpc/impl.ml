open Lwt.Infix

let max_log_chunk_size = 102400L  (* 100K at a time *)

let read ~start path =
  let ch = open_in_bin (Fpath.to_string path) in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let len = LargeFile.in_channel_length ch in
  let (+) = Int64.add in
  let (-) = Int64.sub in
  let start = if start < 0L then len + start else start in
  let start = if start < 0L then 0L else if start > len then len else start in
  LargeFile.seek_in ch start;
  let len = min max_log_chunk_size (len - start) in
  really_input_string ch (Int64.to_int len), start + len

(** Functor that takes both Current and Db modules.
    Used internally and for testing with mock databases. *)
module Make_with_db (Current : S.CURRENT) (Db : S.DB) = struct
  open Capnp_rpc_lwt

  (* Helper functions for level conversion *)
  let level_to_capnp level =
    let s = Current.Level.to_string level in
    match s with
    | "harmless" -> Api.Builder.ConfirmLevel.Harmless
    | "mostly-harmless" -> Api.Builder.ConfirmLevel.MostlyHarmless
    | "average" -> Api.Builder.ConfirmLevel.Average
    | "above-average" -> Api.Builder.ConfirmLevel.AboveAverage
    | "dangerous" -> Api.Builder.ConfirmLevel.Dangerous
    | _ -> Api.Builder.ConfirmLevel.Harmless  (* fallback *)

  let capnp_to_level = function
    | Api.Reader.ConfirmLevel.Harmless -> Current.Level.of_string "harmless"
    | Api.Reader.ConfirmLevel.MostlyHarmless -> Current.Level.of_string "mostly-harmless"
    | Api.Reader.ConfirmLevel.Average -> Current.Level.of_string "average"
    | Api.Reader.ConfirmLevel.AboveAverage -> Current.Level.of_string "above-average"
    | Api.Reader.ConfirmLevel.Dangerous -> Current.Level.of_string "dangerous"
    | Api.Reader.ConfirmLevel.Undefined _ -> Error (`Msg "undefined level")

  module Job = struct
    let job_cache = ref Current.Job.Map.empty

    let stream_log_data ~job_id ~start =
      match Current.Job.log_path job_id with
      | Error `Msg m -> Lwt_result.fail (`Capnp (`Exception (Capnp_rpc.Exception.v m)))
      | Ok path ->
        let rec aux () =
          match read ~start path with
          | ("", _) as x ->
            begin match Current.Job.lookup_running job_id with
              | None -> Lwt_result.return x
              | Some job ->
                Current.Job.wait_for_log_data job >>= aux
            end
          | x -> Lwt_result.return x
        in
        aux ()

    let rec local engine job_id =
      let module Job = Api.Service.Job in
      match Current.Job.Map.find_opt job_id !job_cache with
      | Some job ->
        Capability.inc_ref job;
        job
      | None ->
        let cap =
          let lookup () =
            let state = Current.Engine.state engine in
            Current.Job.Map.find_opt job_id (state.jobs)
          in
          Job.local @@ object
            inherit Job.service

            method log_impl params release_param_caps =
              let open Job.Log in
              release_param_caps ();
              let start = Params.start_get params in
              Log.info (fun f -> f "log(%S, %Ld)" job_id start);
              Service.return_lwt @@ fun () ->
              stream_log_data ~job_id ~start >|= function
              | Error _ as e -> e
              | Ok (log, next) ->
                let response, results = Service.Response.create Results.init_pointer in
                Results.log_set results log;
                Results.next_set results next;
                Ok response

            method rebuild_impl _params release_param_caps =
              release_param_caps ();
              Log.info (fun f -> f "rebuild(%S)" job_id);
              match lookup () with
              | None -> Service.fail "Job is no longer active (cannot rebuild)"
              | Some job ->
                match job#rebuild with
                | None -> Service.fail "Job cannot be rebuilt at the moment"
                | Some rebuild ->
                  let open Job.Rebuild in
                  let response, results = Service.Response.create Results.init_pointer in
                  let new_job = local engine (rebuild ()) in
                  Results.job_set results (Some new_job);
                  Capability.dec_ref new_job;
                  Service.return_lwt @@ fun () ->
                  (* Allow the engine to re-evaluate, so the job will appear
                     active to the caller immediately. *)
                  Lwt.pause () >|= fun () ->
                  Ok response

            method cancel_impl _params release_param_caps =
              release_param_caps ();
              Log.info (fun f -> f "cancel(%S)" job_id);
              match Current.Job.lookup_running job_id with
              | None -> Service.fail "Job is no longer active (cannot cancel)"
              | Some job ->
                Current.Job.cancel job "Cancelled by user";
                Service.return_empty ()

            method status_impl _params release_param_caps =
              let open Job.Status in
              release_param_caps ();
              Log.info (fun f -> f "status(%S)" job_id);
              let response, results = Service.Response.create Results.init_pointer in
              Results.id_set results job_id;
              let can_cancel =
                match Current.Job.lookup_running job_id with
                | Some job -> Current.Job.cancelled_state job = Ok ()
                | None -> false
              in
              begin match lookup () with
                | None -> Results.description_set results "Inactive job"
                | Some job ->
                  Results.description_set results (Fmt.str "%t" job#pp);
                  Results.can_cancel_set results can_cancel;
                  Results.can_rebuild_set results (job#rebuild <> None);
              end;
              Service.return response

            method approve_early_start_impl _params release_param_caps =
              release_param_caps ();
              Log.info (fun f -> f "approveEarlyStart(%S)" job_id);
              match Current.Job.lookup_running job_id with
              | None -> Service.fail "Job is not running (cannot approve early start)"
              | Some job ->
                let response = Service.Response.create_empty () in
                Current.Job.approve_early_start job;
                Service.return response

            method! release =
              job_cache := Current.Job.Map.remove job_id !job_cache
          end
        in
        job_cache := Current.Job.Map.add job_id cap !job_cache;
        cap

    let local_opt engine job_id =
      match Current.Job.log_path job_id with
      | Error _ as e -> e
      | Ok _ -> Ok (local engine job_id)
  end

  let job ~engine id = Job.local engine id

  let engine engine =
    let module Engine = Api.Service.Engine in
    Engine.local @@ object
      inherit Engine.service

      (* Existing methods *)

      method active_jobs_impl _params release_param_caps =
        let open Engine.ActiveJobs in
        release_param_caps ();
        Log.info (fun f -> f "activeJobs");
        let response, results = Service.Response.create Results.init_pointer in
        let state = Current.Engine.state engine in
        Current.Job.Map.bindings (state.jobs)
        |> List.map fst |> Results.ids_set_list results |> ignore;
        Service.return response

      method job_impl params release_param_caps =
        let open Engine.Job in
        let id = Params.id_get params in
        Log.info (fun f -> f "job(%S)" id);
        release_param_caps ();
        let response, results = Service.Response.create Results.init_pointer in
        match Job.local_opt engine id with
        | Error `Msg m -> Service.fail "%s" m
        | Ok job ->
          Results.job_set results (Some job);
          Capability.dec_ref job;
          Service.return response

      (* Database query methods - fully implemented with Db module *)

      method query_impl params release_param_caps =
        let open Engine.Query in
        release_param_caps ();
        Log.info (fun f -> f "query");
        let p = Params.params_get params in
        let op = match Api.Reader.QueryParams.op_get p with "" -> None | s -> Some s in
        let ok = match Api.Reader.OptBool.get (Api.Reader.QueryParams.ok_get p) with
          | Api.Reader.OptBool.Unset -> None
          | Api.Reader.OptBool.Value v -> Some v
          | Api.Reader.OptBool.Undefined _ -> None
        in
        let rebuild = match Api.Reader.OptBool.get (Api.Reader.QueryParams.rebuild_get p) with
          | Api.Reader.OptBool.Unset -> None
          | Api.Reader.OptBool.Value v -> Some v
          | Api.Reader.OptBool.Undefined _ -> None
        in
        let job_prefix = match Api.Reader.QueryParams.job_prefix_get p with "" -> None | s -> Some s in
        let entries = Db.query ?op ?ok ?rebuild ?job_prefix () in
        let response, results = Service.Response.create Results.init_pointer in
        let arr = Results.entries_init results (List.length entries) in
        entries |> List.iteri (fun i (entry : Db.entry) ->
          let e = Capnp.Array.get arr i in
          Api.Builder.JobHistoryEntry.job_id_set e entry.job_id;
          Api.Builder.JobHistoryEntry.build_set e entry.build;
          Api.Builder.JobHistoryEntry.ready_set e entry.ready;
          Api.Builder.JobHistoryEntry.running_set e (Option.value entry.running ~default:0.0);
          Api.Builder.JobHistoryEntry.finished_set e entry.finished;
          Api.Builder.JobHistoryEntry.rebuild_set e entry.rebuild;
          let outcome = Api.Builder.JobHistoryEntry.outcome_init e in
          match entry.outcome with
          | Ok value -> Api.Builder.Outcome.success_set outcome value
          | Error (`Msg msg) -> Api.Builder.Outcome.failure_set outcome msg
        );
        Service.return response

      method ops_impl _params release_param_caps =
        let open Engine.Ops in
        release_param_caps ();
        Log.info (fun f -> f "ops");
        let response, results = Service.Response.create Results.init_pointer in
        Db.ops () |> Results.ops_set_list results |> ignore;
        Service.return response

      (* Pipeline overview methods *)

      method pipeline_stats_impl _params release_param_caps =
        let open Engine.PipelineStats in
        release_param_caps ();
        Log.info (fun f -> f "pipelineStats");
        let pipeline = Current.Engine.pipeline engine in
        (* Analysis.stat returns a stats record which we convert to capnp *)
        let stats = Current.Analysis.stat pipeline in
        let response, results = Service.Response.create Results.init_pointer in
        let s = Results.stats_init results in
        Api.Builder.PipelineStats.ok_set_int_exn s stats.ok;
        Api.Builder.PipelineStats.waiting_for_confirmation_set_int_exn s stats.waiting_for_confirmation;
        Api.Builder.PipelineStats.ready_set_int_exn s stats.ready;
        Api.Builder.PipelineStats.running_set_int_exn s stats.running;
        Api.Builder.PipelineStats.failed_set_int_exn s stats.failed;
        Api.Builder.PipelineStats.blocked_set_int_exn s stats.blocked;
        Service.return response

      method pipeline_state_impl _params release_param_caps =
        let open Engine.PipelineState in
        release_param_caps ();
        Log.info (fun f -> f "pipelineState");
        let state = Current.Engine.state engine in
        let response, results = Service.Response.create Results.init_pointer in
        let s = Results.state_init results in
        (match state.value with
         | Ok () -> Api.Builder.PipelineState.success_set s
         | Error (`Msg msg) -> Api.Builder.PipelineState.failed_set s msg
         | Error (`Active `Ready) ->
           Api.Builder.PipelineState.active_set s Api.Builder.ActiveState.Ready
         | Error (`Active `Running) ->
           Api.Builder.PipelineState.active_set s Api.Builder.ActiveState.Running
         | Error (`Active `Waiting_for_confirmation) ->
           Api.Builder.PipelineState.active_set s Api.Builder.ActiveState.WaitingForConfirmation
        );
        Service.return response

      method pipeline_dot_impl _params release_param_caps =
        let open Engine.PipelineDot in
        release_param_caps ();
        Log.info (fun f -> f "pipelineDot");
        let pipeline = Current.Engine.pipeline engine in
        let collapse_link ~k:_ ~v:_ = None in
        let job_info (meta : Current.Metadata.t) =
          let url = meta.job_id |> Option.map (fun id -> Printf.sprintf "/job/%s" id) in
          meta.update, url
        in
        let dot = Fmt.to_to_string
          (Current.Analysis.pp_dot ~env:[] ~collapse_link ~job_info) pipeline in
        let response, results = Service.Response.create Results.init_pointer in
        Results.dot_set results dot;
        Service.return response

      method get_confirm_level_impl _params release_param_caps =
        let open Engine.GetConfirmLevel in
        release_param_caps ();
        Log.info (fun f -> f "getConfirmLevel");
        let config = Current.Engine.config engine in
        let response, results = Service.Response.create Results.init_pointer in
        (match Current.Config.get_confirm config with
         | None -> Results.is_set_set results false
         | Some level ->
           Results.is_set_set results true;
           Results.level_set results (level_to_capnp level)
        );
        Service.return response

      method set_confirm_level_impl params release_param_caps =
        let open Engine.SetConfirmLevel in
        release_param_caps ();
        let unset = Params.unset_get params in
        Log.info (fun f -> f "setConfirmLevel(unset=%b)" unset);
        let config = Current.Engine.config engine in
        if unset then begin
          Current.Config.set_confirm config None;
          Service.return_empty ()
        end else begin
          let level_capnp = Params.level_get params in
          match capnp_to_level level_capnp with
          | Error (`Msg msg) -> Service.fail "Invalid confirmation level: %s" msg
          | Ok level ->
            Current.Config.set_confirm config (Some level);
            Service.return_empty ()
        end

      method rebuild_all_impl params release_param_caps =
        let open Engine.RebuildAll in
        release_param_caps ();
        let job_ids = Params.job_ids_get_list params in
        Log.info (fun f -> f "rebuildAll(%d jobs)" (List.length job_ids));
        let state = Current.Engine.state engine in
        let jobs = state.jobs in
        let succeeded, failed =
          job_ids |> List.partition_map (fun job_id ->
            match Current.Job.Map.find_opt job_id jobs with
            | None -> Right job_id
            | Some actions ->
              match actions#rebuild with
              | None -> Right job_id
              | Some rebuild ->
                let _new_id : string = rebuild () in
                Left job_id
          )
        in
        let response, results = Service.Response.create Results.init_pointer in
        succeeded |> Results.succeeded_set_list results |> ignore;
        failed |> Results.failed_set_list results |> ignore;
        Service.return response
    end
end

(** Main functor that uses Current_cache.Db for job history queries. *)
module Make (Current : S.CURRENT) = Make_with_db(Current)(Current_cache.Db)
