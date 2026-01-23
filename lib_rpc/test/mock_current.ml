(** Mock implementation of Current for testing the RPC layer *)

(* Re-export types from S for convenience *)
type active = Current_rpc.S.active
type 'a output = 'a Current_rpc.S.output
type stats = Current_rpc.S.stats

(* Mock term type - just stores a result *)
type 'a term = {
  value : 'a output;
}

let return x = { value = Ok x }
let fail msg = { value = Error (`Msg msg) }
let active state = { value = Error (`Active state) }

class type actions = object
  method pp : Format.formatter -> unit
  method rebuild : (unit -> string) option
end

module Job = struct
  type t = {
    id : string;
    log : string;
    mutable cancelled : (unit, [`Msg of string]) result;
  }

  module Map = Map.Make(String)

  (* Simulated job storage *)
  let jobs : t Map.t ref = ref Map.empty
  let log_dir = ref "/tmp/mock-jobs"

  let reset () =
    jobs := Map.empty

  let create_job ~id ~log =
    let job = { id; log; cancelled = Ok () } in
    jobs := Map.add id job !jobs;
    (* Create the log file *)
    let path = Fpath.(v !log_dir / id) in
    let _ = Bos.OS.Dir.create (Fpath.parent path) in
    let _ = Bos.OS.File.write path log in
    job

  let log_path job_id =
    let path = Fpath.(v !log_dir / job_id) in
    if Map.mem job_id !jobs then Ok path
    else Error (`Msg (Printf.sprintf "Unknown job: %s" job_id))

  let lookup_running job_id =
    Map.find_opt job_id !jobs

  let wait_for_log_data _job =
    Lwt.return_unit

  let approve_early_start _job = ()

  let cancel job msg =
    job.cancelled <- Error (`Msg msg)

  let cancelled_state job = job.cancelled
end

module Level = struct
  type t =
    | Harmless
    | Mostly_harmless
    | Average
    | Above_average
    | Dangerous

  let values = [Harmless; Mostly_harmless; Average; Above_average; Dangerous]

  let to_string = function
    | Harmless -> "harmless"
    | Mostly_harmless -> "mostly-harmless"
    | Average -> "average"
    | Above_average -> "above-average"
    | Dangerous -> "dangerous"

  let of_string = function
    | "harmless" -> Ok Harmless
    | "mostly-harmless" -> Ok Mostly_harmless
    | "average" -> Ok Average
    | "above-average" -> Ok Above_average
    | "dangerous" -> Ok Dangerous
    | s -> Error (`Msg (Printf.sprintf "Unknown level: %s" s))
end

module Config = struct
  type t = {
    mutable confirm : Level.t option;
  }

  let create ?confirm () = { confirm }

  let get_confirm t = t.confirm
  let set_confirm t level = t.confirm <- level
end

module Metadata = struct
  type t = {
    job_id : string option;
    update : active option;
  }
end

(* Mock analysis - returns configurable stats *)
module Analysis = struct
  let mock_stats : stats ref = ref {
    Current_term.S.ok = 5;
    waiting_for_confirmation = 1;
    ready = 2;
    running = 3;
    failed = 1;
    blocked = 2;
  }

  let set_stats (s : stats) = mock_stats := s

  let stat (_term : 'a term) : stats = !mock_stats

  let pp_dot ~env:_ ~collapse_link:_ ~job_info:_ ppf (_term : 'a term) =
    Fmt.pf ppf "digraph pipeline {@,  node1 [label=\"mock\"];@,}@."
end

module Engine = struct
  type t = {
    config : Config.t;
    mutable value : unit output;
    mutable jobs : actions Job.Map.t;
    mutable pipeline : unit term;
  }

  type results = {
    value : unit output;
    jobs : actions Job.Map.t;
  }

  let create ?(config = Config.create ()) () =
    {
      config;
      value = Ok ();
      jobs = Job.Map.empty;
      pipeline = return ();
    }

  let state (t : t) : results = { value = t.value; jobs = t.jobs }

  let config (t : t) = t.config

  let pipeline (t : t) = t.pipeline

  let set_value (t : t) (value : unit output) = t.value <- value
  let set_pipeline (t : t) (p : unit term) = t.pipeline <- p

  let add_job (t : t) job_id (actions : actions) =
    t.jobs <- Job.Map.add job_id actions t.jobs

  let remove_job (t : t) job_id =
    t.jobs <- Job.Map.remove job_id t.jobs
end

(* Mock database module *)
module Mock_db = struct
  type entry = {
    job_id : string;
    build : int64;
    value : string;
    outcome : (string, [`Msg of string]) result;
    ready : float;
    running : float option;
    finished : float;
    rebuild : bool;
  }

  let entries : entry list ref = ref []

  let reset () = entries := []

  let add_entry entry =
    entries := entry :: !entries

  let query ?op ?ok ?rebuild ?job_prefix () =
    !entries |> List.filter (fun e ->
      (match op with None -> true | Some op -> String.length e.job_id > 0 && op = "mock-op") &&
      (match ok with None -> true | Some ok -> (Result.is_ok e.outcome) = ok) &&
      (match rebuild with None -> true | Some r -> e.rebuild = r) &&
      (match job_prefix with None -> true | Some p ->
        String.length e.job_id >= String.length p &&
        String.sub e.job_id 0 (String.length p) = p)
    )

  let ops () = ["docker-build"; "git-clone"; "test-run"]
end
