(* Run this to connect to the server in "rpc_server.ml".

   The first argument is the path to the engine.cap file written by the server.
   This file tells the client how to connect and gives it the secret token to allow access.

   If run without further arguments, it queries the server and displays a list of active jobs, e.g.

   $ dune exec -- rpc_client ./engine.cap
   2019-09-19/103850-docker-build-11b894
   2019-09-19/104747-docker-run-01a31e

   With a job ID, it shows information about that job:

   $ dune exec -- rpc_client ./engine.cap 2019-09-19/103850-docker-build-11b894
   Job "2019-09-19/103850-docker-build-11b894":
     Description: docker build {
                                 "commit": "946ce0334a15251459d1821c2b63e240d2760106",
                                 "dockerfile": null,
                                 "docker_context": null,
                                 "squash": false
                               } (completed)
     Can cancel: false
     Can rebuild: true

   You can also pass the method as a further argument, e.g.

   $ dune exec -- rpc_client ./engine.cap 2019-09-19/103850-docker-build-11b894 rebuild
   Rebuild scheduled
*)

open Lwt.Infix
open Capnp_rpc_lwt

let () = Prometheus_unix.Logging.init ~default_level:Logs.Warning ()

let list_jobs engine =
  Current_rpc.Engine.active_jobs engine |> Lwt_result.map @@ fun jobs ->
  List.iter print_endline jobs

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

let start job =
  Current_rpc.Job.approve_early_start job >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    Fmt.pr "Job is now approved to start without waiting for confirmation.@.";
    show_log job

let rebuild job =
  Fmt.pr "Requesting rebuild...@.";
  let new_job = Current_rpc.Job.rebuild job in
  show_log new_job

let main ?job_id ~job_op engine_url =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Vat.import_exn vat engine_url in
  Sturdy_ref.connect_exn sr >>= fun engine ->
  Lwt.finalize (fun () ->
      match job_id with
      | None -> list_jobs engine
      | Some job_id ->
        let job = Current_rpc.Engine.job engine job_id in
        Lwt.finalize
          (fun () -> job_op job)
          (fun () -> Capability.dec_ref job; Lwt.return_unit)
    )
    (fun () ->
       Capability.dec_ref engine;
       Lwt.return_unit
    )

(* Command-line parsing *)

open Cmdliner

let cap =
  Arg.required @@
  Arg.pos 0 Arg.(some Capnp_rpc_unix.sturdy_uri) None @@
  Arg.info
    ~doc:"The engine.cap file."
    ~docv:"CAP"
    []

let job =
  Arg.value @@
  Arg.pos 1 Arg.(some string) None @@
  Arg.info
    ~doc:"The job ID to act upon. If not given, it shows a list of active job IDs."
    ~docv:"JOB"
    []

let job_op =
  let ops = [
    "log", `Show_log;
    "status", `Show_status;
    "cancel", `Cancel;
    "rebuild", `Rebuild;
    "start", `Start;
  ] in
  Arg.value @@
  Arg.pos 2 Arg.(enum ops) `Show_status @@
  Arg.info
    ~doc:"The operation to perform (log, status, cancel, rebuild or start)."
    ~docv:"METHOD"
    []

(* (cmdliner's [enum] can't cope with functions) *)
let to_fn = function
  | `Cancel -> cancel
  | `Rebuild -> rebuild
  | `Show_log -> show_log
  | `Show_status -> show_status
  | `Start -> start

let cmd =
  let doc = "Client for rpc_server.ml" in
  let main engine job_id job_op =
    let job_op = to_fn job_op in
    match Lwt_main.run (main ?job_id ~job_op engine) with
    | Error `Capnp ex -> Fmt.error_msg "%a" Capnp_rpc.Error.pp ex
    | Ok () | Error `Msg _ as x -> x
  in
  Term.(term_result (const main $ cap $ job $ job_op)),
  Term.info "rpc_client" ~doc

let () = Term.(exit @@ eval cmd)
