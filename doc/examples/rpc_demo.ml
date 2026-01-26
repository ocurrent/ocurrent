(* Self-contained RPC demo that runs both server and client in-process.

   This demonstrates the extended RPC API without requiring external setup.

   Run with:
     dune exec -- rpc_demo
*)

open Lwt.Infix

(* Create a minimal mock implementation for demonstration.
   Note: not constrained here so internal functions are accessible;
   signature is verified when passed to functor. *)
module Demo_current = struct
  type 'a term = { _term_value : ('a, [`Active of [`Ready | `Running | `Waiting_for_confirmation] | `Msg of string]) result }

  class type actions = object
    method pp : Format.formatter -> unit
    method rebuild : (unit -> string) option
  end

  module Job = struct
    type t = { _id : string; mutable cancelled : bool }
    module Map = Map.Make(String)

    let jobs : t Map.t ref = ref Map.empty
    let log_dir = "/tmp/rpc-demo-logs"

    let log_path job_id =
      let path = Fpath.(v log_dir / job_id) in
      if Map.mem job_id !jobs then Ok path
      else Error (`Msg ("Unknown job: " ^ job_id))

    let lookup_running job_id = Map.find_opt job_id !jobs
    let wait_for_log_data _ = Lwt.return_unit
    let approve_early_start _ = ()
    let cancel job _ = job.cancelled <- true
    let cancelled_state job = if job.cancelled then Error (`Msg "Cancelled") else Ok ()

    let add_job id =
      let job = { _id = id; cancelled = false } in
      jobs := Map.add id job !jobs;
      (* Create log file *)
      (try Unix.mkdir log_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      let path = Filename.concat log_dir id in
      let oc = open_out path in
      Printf.fprintf oc "=== Log for job %s ===\nStarted at %s\nRunning...\nCompleted successfully.\n"
        id (string_of_float (Unix.time ()));
      close_out oc;
      job
  end

  module Level = struct
    type t = Harmless | Mostly_harmless | Average | Above_average | Dangerous
    let values = [Harmless; Mostly_harmless; Average; Above_average; Dangerous]
    let to_string = function
      | Harmless -> "harmless" | Mostly_harmless -> "mostly-harmless"
      | Average -> "average" | Above_average -> "above-average" | Dangerous -> "dangerous"
    let of_string = function
      | "harmless" -> Ok Harmless | "mostly-harmless" -> Ok Mostly_harmless
      | "average" -> Ok Average | "above-average" -> Ok Above_average
      | "dangerous" -> Ok Dangerous | s -> Error (`Msg ("Unknown: " ^ s))
  end

  module Config = struct
    type t = { mutable confirm : Level.t option }
    let v = { confirm = Some Level.Average }
    let get_confirm t = t.confirm
    let set_confirm t level = t.confirm <- level
  end

  module Metadata = struct
    type t = { job_id : string option; update : [`Ready | `Running | `Waiting_for_confirmation] option }
  end

  module Analysis = struct
    let stat _ = { Current_term.S.ok = 5; waiting_for_confirmation = 1; ready = 2; running = 3; failed = 1; blocked = 2 }
    let pp_dot ~env:_ ~collapse_link:_ ~job_info:_ ppf _ =
      Fmt.pf ppf "digraph pipeline {\n  rankdir=LR;\n  node [shape=box];\n";
      Fmt.pf ppf "  fetch [label=\"git fetch\" style=filled fillcolor=\"#90ee90\"];\n";
      Fmt.pf ppf "  build [label=\"docker build\" style=filled fillcolor=\"#ffa500\"];\n";
      Fmt.pf ppf "  test [label=\"run tests\" style=filled fillcolor=\"#ffff00\"];\n";
      Fmt.pf ppf "  fetch -> build -> test;\n";
      Fmt.pf ppf "}\n"
  end

  module Engine = struct
    type t = {
      config : Config.t;
      mutable engine_jobs : actions Job.Map.t;
    }
    type results = {
      value : unit Current_rpc.S.output;
      jobs : actions Job.Map.t;
    }
    let v : t = { config = Config.v; engine_jobs = Job.Map.empty }
    let state (t : t) : results = { value = Error (`Active `Running); jobs = t.engine_jobs }
    let config (t : t) = t.config
    let pipeline (_ : t) = { _term_value = Error (`Active `Running) }

    let add_job id rebuild_fn =
      let actions = object
        method pp fmt = Fmt.pf fmt "Demo job: %s" id
        method rebuild = rebuild_fn
      end in
      v.engine_jobs <- Job.Map.add id actions v.engine_jobs
  end
end

(* Mock database with sample data *)
module Demo_db : Current_rpc.S.DB = struct
  type entry = {
    job_id : string; build : int64; value : string;
    outcome : (string, [`Msg of string]) result;
    ready : float; running : float option; finished : float; rebuild : bool;
  }

  let sample_entries = [
    { job_id = "docker-build-abc123"; build = 1L; value = "sha256:abc123";
      outcome = Ok "sha256:abc123"; ready = 1705312800.0; running = Some 1705312810.0;
      finished = 1705312900.0; rebuild = false };
    { job_id = "git-fetch-def456"; build = 2L; value = "commit:def456";
      outcome = Ok "commit:def456"; ready = 1705312700.0; running = Some 1705312705.0;
      finished = 1705312750.0; rebuild = false };
    { job_id = "docker-build-old"; build = 1L; value = "";
      outcome = Error (`Msg "Build failed: missing dependency"); ready = 1705226400.0;
      running = Some 1705226410.0; finished = 1705226500.0; rebuild = true };
  ]

  let query ?op:_ ?ok ?rebuild:_ ?job_prefix () =
    sample_entries |> List.filter (fun e ->
      (match ok with None -> true | Some ok -> (Result.is_ok e.outcome) = ok) &&
      (match job_prefix with None -> true | Some p ->
        String.length e.job_id >= String.length p &&
        String.sub e.job_id 0 (String.length p) = p))

  let ops () = ["docker-build"; "git-fetch"; "test-run"]
end

module Rpc = Current_rpc.Impl_with_db(Demo_current)(Demo_db)

(* Helper for timestamps *)
let pp_time ppf t =
  let open Unix in
  let tm = localtime t in
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let () = Prometheus_unix.Logging.init ~default_level:Logs.Warning ()

let run () =
  (* Setup demo data *)
  let _ = Demo_current.Job.add_job "docker-build-abc123" in
  let _ = Demo_current.Job.add_job "git-fetch-def456" in
  Demo_current.Engine.add_job "docker-build-abc123" (Some (fun () -> "new-job-id"));
  Demo_current.Engine.add_job "git-fetch-def456" None;

  (* Create the RPC service *)
  let service = Rpc.engine Demo_current.Engine.v in

  Fmt.pr "=== OCurrent Extended RPC Demo ===@.@.";

  (* 1. Active Jobs *)
  Fmt.pr "--- Active Jobs ---@.";
  Current_rpc.Engine.active_jobs service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok jobs ->
      List.iter (fun j -> Fmt.pr "  %s@." j) jobs;
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 2. Pipeline Stats *)
  Fmt.pr "--- Pipeline Statistics ---@.";
  Current_rpc.Engine.pipeline_stats service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok stats ->
      Fmt.pr "  OK:                       %d@." stats.ok;
      Fmt.pr "  Waiting for confirmation: %d@." stats.waiting_for_confirmation;
      Fmt.pr "  Ready:                    %d@." stats.ready;
      Fmt.pr "  Running:                  %d@." stats.running;
      Fmt.pr "  Failed:                   %d@." stats.failed;
      Fmt.pr "  Blocked:                  %d@." stats.blocked;
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 3. Pipeline State *)
  Fmt.pr "--- Pipeline State ---@.";
  Current_rpc.Engine.pipeline_state service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok state ->
      let s = match state with
        | Current_rpc.Engine.Success -> "SUCCESS"
        | Current_rpc.Engine.Failed msg -> "FAILED: " ^ msg
        | Current_rpc.Engine.Active `Ready -> "ACTIVE (ready)"
        | Current_rpc.Engine.Active `Running -> "ACTIVE (running)"
        | Current_rpc.Engine.Active `Waiting_for_confirmation -> "ACTIVE (waiting)"
      in
      Fmt.pr "  State: %s@." s;
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 4. Operation Types *)
  Fmt.pr "--- Operation Types ---@.";
  Current_rpc.Engine.ops service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok ops ->
      List.iter (fun op -> Fmt.pr "  %s@." op) ops;
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 5. Job History Query *)
  Fmt.pr "--- Job History (all) ---@.";
  let params = { Current_rpc.Engine.op = None; ok = None; rebuild = None; job_prefix = None } in
  Current_rpc.Engine.query service params >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok entries ->
      entries |> List.iter (fun (e : Current_rpc.Engine.history_entry) ->
        let outcome = match e.outcome with Ok v -> "OK: " ^ v | Error msg -> "FAILED: " ^ msg in
        Fmt.pr "  %s (build #%Ld)@." e.job_id e.build;
        Fmt.pr "    %s@." outcome;
        Fmt.pr "    finished: %a@." pp_time e.finished);
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 6. Job History Query (filtered) *)
  Fmt.pr "--- Job History (docker- prefix only) ---@.";
  let params = { Current_rpc.Engine.op = None; ok = None; rebuild = None; job_prefix = Some "docker-" } in
  Current_rpc.Engine.query service params >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok entries ->
      Fmt.pr "  Found %d entries@." (List.length entries);
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 7. Confirmation Level *)
  Fmt.pr "--- Confirmation Level ---@.";
  Current_rpc.Engine.get_confirm_level service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok level ->
      let s = match level with
        | None -> "disabled"
        | Some Current_rpc.Engine.Harmless -> "harmless"
        | Some Current_rpc.Engine.Mostly_harmless -> "mostly-harmless"
        | Some Current_rpc.Engine.Average -> "average"
        | Some Current_rpc.Engine.Above_average -> "above-average"
        | Some Current_rpc.Engine.Dangerous -> "dangerous"
      in
      Fmt.pr "  Current level: %s@." s;
      Lwt.return_unit) >>= fun () ->

  Fmt.pr "  Setting level to 'dangerous'...@.";
  Current_rpc.Engine.set_confirm_level service (Some Current_rpc.Engine.Dangerous) >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok () -> Fmt.pr "  Done.@."; Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  (* 8. Pipeline DOT Graph *)
  Fmt.pr "--- Pipeline DOT Graph ---@.";
  Current_rpc.Engine.pipeline_dot service >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok dot ->
      Fmt.pr "%s@." dot;
      Lwt.return_unit) >>= fun () ->

  (* 9. Job Log *)
  Fmt.pr "--- Job Log (first job) ---@.";
  let job = Current_rpc.Engine.job service "docker-build-abc123" in
  Current_rpc.Job.log ~start:0L job >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok (data, _next) ->
      Fmt.pr "%s@." data;
      Lwt.return_unit) >>= fun () ->
  Capnp_rpc_lwt.Capability.dec_ref job;

  (* 10. Rebuild All *)
  Fmt.pr "--- Rebuild All ---@.";
  Current_rpc.Engine.rebuild_all service
    ["docker-build-abc123"; "git-fetch-def456"; "nonexistent"]
  >>= (function
    | Error `Capnp e -> Fmt.pr "Error: %a@." Capnp_rpc.Error.pp e; Lwt.return_unit
    | Ok result ->
      Fmt.pr "  Succeeded: %s@." (String.concat ", " result.succeeded);
      Fmt.pr "  Failed: %s@." (String.concat ", " result.failed);
      Lwt.return_unit) >>= fun () ->
  Fmt.pr "@.";

  Fmt.pr "=== Demo Complete ===@.";
  Capnp_rpc_lwt.Capability.dec_ref service;
  Lwt.return_unit

let () = Lwt_main.run (run ())
