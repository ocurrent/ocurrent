open Capnp_rpc_lwt

type t = Api.Service.Engine.t Capability.t

module Engine = Api.Client.Engine

(* Existing methods *)

let active_jobs t =
  let open Engine.ActiveJobs in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map Results.ids_get_list

let job t id =
  let open Engine.Job in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_caps t method_id request Results.job_get_pipelined

(* New types *)

type query_params = {
  op : string option;
  ok : bool option;
  rebuild : bool option;
  job_prefix : string option;
}

type history_entry = {
  job_id : string;
  build : int64;
  outcome : (string, string) result;
  ready : float;
  running : float option;
  finished : float;
  rebuild : bool;
}

type stats = {
  ok : int;
  waiting_for_confirmation : int;
  ready : int;
  running : int;
  failed : int;
  blocked : int;
}

type pipeline_state =
  | Success
  | Failed of string
  | Active of [ `Ready | `Running | `Waiting_for_confirmation ]

type confirm_level = Harmless | Mostly_harmless | Average | Above_average | Dangerous

type rebuild_result = {
  succeeded : string list;
  failed : string list;
}

(* Helper functions *)

let set_opt_bool builder = function
  | None -> Api.Builder.OptBool.unset_set builder
  | Some v -> Api.Builder.OptBool.value_set builder v

let history_entry_of_capnp entry =
  let outcome =
    match Api.Reader.Outcome.get (Api.Reader.JobHistoryEntry.outcome_get entry) with
    | Api.Reader.Outcome.Success v -> Ok v
    | Api.Reader.Outcome.Failure e -> Error e
    | Api.Reader.Outcome.Undefined _ -> Error "unknown outcome"
  in
  let running =
    let r = Api.Reader.JobHistoryEntry.running_get entry in
    if r = 0.0 then None else Some r
  in
  {
    job_id = Api.Reader.JobHistoryEntry.job_id_get entry;
    build = Api.Reader.JobHistoryEntry.build_get entry;
    outcome;
    ready = Api.Reader.JobHistoryEntry.ready_get entry;
    running;
    finished = Api.Reader.JobHistoryEntry.finished_get entry;
    rebuild = Api.Reader.JobHistoryEntry.rebuild_get entry;
  }

let level_to_capnp = function
  | Harmless -> Api.Builder.ConfirmLevel.Harmless
  | Mostly_harmless -> Api.Builder.ConfirmLevel.MostlyHarmless
  | Average -> Api.Builder.ConfirmLevel.Average
  | Above_average -> Api.Builder.ConfirmLevel.AboveAverage
  | Dangerous -> Api.Builder.ConfirmLevel.Dangerous

let capnp_to_level = function
  | Api.Reader.ConfirmLevel.Harmless -> Harmless
  | Api.Reader.ConfirmLevel.MostlyHarmless -> Mostly_harmless
  | Api.Reader.ConfirmLevel.Average -> Average
  | Api.Reader.ConfirmLevel.AboveAverage -> Above_average
  | Api.Reader.ConfirmLevel.Dangerous -> Dangerous
  | Api.Reader.ConfirmLevel.Undefined _ -> Harmless  (* fallback *)

(* New methods *)

let query t params =
  let open Engine.Query in
  let request, p = Capability.Request.create Params.init_pointer in
  let qp = Params.params_init p in
  (match params.op with None -> () | Some op -> Api.Builder.QueryParams.op_set qp op);
  set_opt_bool (Api.Builder.QueryParams.ok_init qp) params.ok;
  set_opt_bool (Api.Builder.QueryParams.rebuild_init qp) params.rebuild;
  (match params.job_prefix with None -> () | Some jp -> Api.Builder.QueryParams.job_prefix_set qp jp);
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun results ->
  Results.entries_get_list results |> List.map history_entry_of_capnp

let ops t =
  let open Engine.Ops in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map Results.ops_get_list

let pipeline_stats t =
  let open Engine.PipelineStats in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun results ->
  let s = Results.stats_get results in
  {
    ok = Api.Reader.PipelineStats.ok_get_int_exn s;
    waiting_for_confirmation = Api.Reader.PipelineStats.waiting_for_confirmation_get_int_exn s;
    ready = Api.Reader.PipelineStats.ready_get_int_exn s;
    running = Api.Reader.PipelineStats.running_get_int_exn s;
    failed = Api.Reader.PipelineStats.failed_get_int_exn s;
    blocked = Api.Reader.PipelineStats.blocked_get_int_exn s;
  }

let pipeline_state t =
  let open Engine.PipelineState in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun results ->
  let s = Results.state_get results in
  match Api.Reader.PipelineState.get s with
  | Api.Reader.PipelineState.Success -> Success
  | Api.Reader.PipelineState.Failed msg -> Failed msg
  | Api.Reader.PipelineState.Active Api.Reader.ActiveState.Ready -> Active `Ready
  | Api.Reader.PipelineState.Active Api.Reader.ActiveState.Running -> Active `Running
  | Api.Reader.PipelineState.Active Api.Reader.ActiveState.WaitingForConfirmation ->
    Active `Waiting_for_confirmation
  | Api.Reader.PipelineState.Active (Api.Reader.ActiveState.Undefined _) ->
    Active `Running  (* Forward compatibility: treat unknown active state as running *)
  | Api.Reader.PipelineState.Undefined _ ->
    Active `Running  (* Forward compatibility: treat unknown state as active/running *)

let pipeline_dot t =
  let open Engine.PipelineDot in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map Results.dot_get

let get_confirm_level t =
  let open Engine.GetConfirmLevel in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun results ->
  if Results.is_set_get results then
    Some (capnp_to_level (Results.level_get results))
  else
    None

let set_confirm_level t level =
  let open Engine.SetConfirmLevel in
  let request, params = Capability.Request.create Params.init_pointer in
  (match level with
   | None -> Params.unset_set params true
   | Some l ->
     Params.unset_set params false;
     Params.level_set params (level_to_capnp l)
  );
  Capability.call_for_unit t method_id request

let rebuild_all t job_ids =
  let open Engine.RebuildAll in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.job_ids_set_list params job_ids |> ignore;
  Capability.call_for_value t method_id request |> Lwt_result.map @@ fun results ->
  {
    succeeded = Results.succeeded_get_list results;
    failed = Results.failed_get_list results;
  }
