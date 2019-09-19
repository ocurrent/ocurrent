open Lwt.Infix
open Capnp_rpc_lwt

type t = Api.Service.Engine.t Capability.t

module Engine = Api.Client.Engine

let active_jobs t =
  let open Engine.ActiveJobs in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request >|= function
  | Error ex -> Error (`Capnp ex)
  | Ok x -> Ok (Results.ids_get_list x)

let job t id =
  let open Engine.Job in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_caps t method_id request Results.job_get_pipelined
