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

module Make (Current : S.CURRENT) = struct
  open Capnp_rpc_lwt

  module Job = struct
    let job_cache = ref Current.Job_map.empty

    let rec local engine job_id =
      let module Job = Api.Service.Job in
      match Current.Job_map.find_opt job_id !job_cache with
      | Some job ->
        Capability.inc_ref job;
        job
      | None ->
        let cap =
          let lookup () =
            let state = Current.Engine.state engine in
            Current.Job_map.find_opt job_id (Current.Engine.jobs state)
          in
          Job.local @@ object
            inherit Job.service

            method log_impl params release_param_caps =
              let open Job.Log in
              release_param_caps ();
              let start = Params.start_get params in
              Log.info (fun f -> f "log(%S, %Ld)" job_id start);
              let response, results = Service.Response.create Results.init_pointer in
              match Current.Job.log_path job_id with
              | Error `Msg m -> Service.fail "%s" m
              | Ok path ->
                match read ~start path with
                | exception ex -> Service.fail "ERROR reading log: %a" Fmt.exn ex
                | log, next ->
                  Results.log_set results log;
                  Results.next_set results next;
                  Service.return response

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
              match lookup () with
              | None -> Service.fail "Job is no longer active (cannot cancel)"
              | Some job ->
                match job#cancel with
                | None -> Service.fail "Job can no longer be cancelled"
                | Some cancel ->
                  cancel ();
                  Service.return_empty ()

            method status_impl _params release_param_caps =
              let open Job.Status in
              release_param_caps ();
              Log.info (fun f -> f "status(%S)" job_id);
              let response, results = Service.Response.create Results.init_pointer in
              Results.id_set results job_id;
              begin match lookup () with
                | None -> Results.description_set results "Inactive job"
                | Some job ->
                  Results.description_set results (Fmt.strf "%t" job#pp);
                  Results.can_cancel_set results (job#cancel <> None);
                  Results.can_rebuild_set results (job#rebuild <> None);
              end;
              Service.return response

            method! release =
              job_cache := Current.Job_map.remove job_id !job_cache
          end
        in
        job_cache := Current.Job_map.add job_id cap !job_cache;
        cap

    let local_opt engine job_id =
      match Current.Job.log_path job_id with
      | Error _ as e -> e
      | Ok _ -> Ok (local engine job_id)
  end

  let engine engine =
    let module Engine = Api.Service.Engine in
    Engine.local @@ object
      inherit Engine.service

      method active_jobs_impl _params release_param_caps =
        let open Engine.ActiveJobs in
        release_param_caps ();
        Log.info (fun f -> f "activeJobs");
        let response, results = Service.Response.create Results.init_pointer in
        let state = Current.Engine.state engine in
        Current.Job_map.bindings (Current.Engine.jobs state)
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
    end
end
