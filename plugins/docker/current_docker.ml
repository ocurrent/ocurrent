open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(unit ":@,") string)

module Make (Host : S.HOST) = struct
  module Image = Image

  module PC = Current_cache.Make(Pull)

  let docker_context = Host.docker_context

  let pull ?label ~schedule tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s" label |>
    let> () = Current.return () in
    PC.get ~schedule Pull.No_context { Pull.Key.docker_context; tag }

  module BC = Current_cache.Make(Build)

  let pp_sp_label = Fmt.(option (prefix sp string))

  let option_map f = function
    | None -> None
    | Some x -> Some (f x)

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit

  let build ?schedule ?timeout ?(squash=false) ?label ?dockerfile ?pool ?(build_args=[]) ~pull src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    let dockerfile =
      match dockerfile with
      | None -> `File (Fpath.v "Dockerfile")
      | Some (`File _ as f) -> f
      | Some (`Contents c) -> `Contents (Dockerfile.string_of_t c)
    in
    BC.get ?schedule { Build.pull; pool; timeout }
      { Build.Key.commit; dockerfile; docker_context; squash; build_args }

  module RC = Current_cache.Make(Run)

  let run ?label ?pool ?(run_args=[]) image ~args  =
    Current.component "run%a" pp_sp_label label |>
    let> image = image in
    RC.get { Run.pool } { Run.Key.image; args; docker_context; run_args }

  module PrC = Current_cache.Make(Pread)

  let pread ?label ?pool ?(run_args=[]) image ~args  =
    Current.component "pread%a" pp_sp_label label |>
    let> image = image in
    PrC.get { Pread.pool } { Pread.Key.image; args; docker_context; run_args }

  module TC = Current_cache.Output(Tag)

  let tag ~tag image =
    Current.component "docker-tag@,%a" pp_tag tag |>
    let> image = image in
    TC.set Tag.No_context { Tag.Key.tag; docker_context } { Tag.Value.image }

  module Push_cache = Current_cache.Output(Push)

  let push ?auth ~tag image =
    Current.component "docker-push@,%a" pp_tag tag |>
    let> image = image in
    Push_cache.set auth { Push.Key.tag; docker_context } { Push.Value.image }

  module SC = Current_cache.Output(Service)

  let service ~name ~image () =
    Current.component "docker-service@,%s" name |>
    let> image = image in
    SC.set Service.No_context { Service.Key.name; docker_context } { Service.Value.image }

  module Cmd = struct
    open Lwt.Infix

    let ( >>!= ) = Lwt_result.bind

    type t = Lwt_process.command

    let docker args =
      Cmd.docker ~docker_context args

    let rm_f id = docker ["container"; "rm"; "-f"; id]
    let kill id = docker ["container"; "kill"; id]

    (* Try to "docker kill $id". If it fails, just log a warning and continue. *)
    let try_kill_container ~job id =
      Current.Process.exec ~cancellable:false ~job (kill id) >|= function
      | Ok () -> ()
      | Error (`Msg m) -> Current.Job.log job "Warning: Failed to kill container %S: %s" id m

    let with_container ~kill_on_cancel ~job t fn =
      Current.Process.check_output ~cancellable:false ~job t >>!= fun id ->
      let id = String.trim id in
      let did_rm = ref false in
      Lwt.catch
        (fun () ->
           begin
             if kill_on_cancel then (
               Current.Job.on_cancel job (fun _ ->
                   if !did_rm = false then try_kill_container ~job id
                   else Lwt.return_unit
                 )
             ) else (
               Lwt.return_unit
             )
           end >>= fun () ->
           fn id)
        (fun ex -> Lwt.return (Fmt.error_msg "with_container: uncaught exception: %a" Fmt.exn ex))
      >>= fun result ->
      did_rm := true;
      Current.Process.exec ~cancellable:false ~job (rm_f id) >|= function
      | Ok () -> result         (* (the common case, where removing the container succeeds) *)
      | Error (`Msg rm_error) as rm_e ->
        match result with
        | Ok _ -> rm_e
        | Error _ as e ->
          (* The job failed, and removing the container failed too.
             Log the second error and return the first. *)
          Current.Job.log job "Failed to remove container %S when job failed: %s" id rm_error;
          e

    let pp = Cmd.pp
  end
end

module Default = Make(struct
    let docker_context = Sys.getenv_opt "DOCKER_CONTEXT"
  end)

module MC = Current_cache.Output(Push_manifest)

let push_manifest ?auth ~tag manifests =
  Current.component "docker-push-manifest@,%a" pp_tag tag |>
  let> manifests = Current.list_seq manifests in
  MC.set auth tag { Push_manifest.Value.manifests }
