open Lwt.Infix
open Capnp_rpc_lwt

type t = Api.Service.Job.t Capability.t

type id = string

type status = {
  id : string;
  description : string;
  can_cancel : bool;
  can_rebuild : bool;
}

module Job = Api.Client.Job

let log t =
  let open Job.Log in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request >|= function
  | Ok x -> Ok (Results.log_get x)
  | Error e -> Error (`Capnp e)

let status t =
  let open Job.Status in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request >|= function
  | Error e -> Error (`Capnp e)
  | Ok x ->
    Ok {
      id = Results.id_get x;
      description = Results.description_get x;
      can_cancel = Results.can_cancel_get x;
      can_rebuild = Results.can_rebuild_get x;
    }

let cancel t =
  let open Job.Cancel in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request >|= function
  | Error e -> Error (`Capnp e)
  | Ok () -> Ok ()

let rebuild t =
  let open Job.Rebuild in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.job_get_pipelined
