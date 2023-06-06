open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(any ":@,") string)

module Raw = struct
  module Image = Image

  module PullC = Current_cache.Make(Pull)

  let pull ~docker_context ~schedule ~proc ?arch ?auth tag =
    PullC.get ~schedule (auth, proc) { Pull.Key.docker_context; tag; arch }

  module PeekC = Current_cache.Make(Peek)

  let peek ~docker_context ~schedule ~arch ~proc tag =
    PeekC.get ~schedule proc { Peek.Key.docker_context; tag; arch }

  module BC = Current_cache.Make(Build)

  let build ~docker_context ?level ?schedule ?timeout ?(squash=false) ?(buildx = false) ?dockerfile ?path ?pool ?(build_args=[]) ~pull ~fs ~proc commit =
    let dockerfile =
      match dockerfile with
      | None -> `File (Fpath.v "Dockerfile")
      | Some (`File _ as f) -> f
      | Some (`Contents c) -> `Contents c
    in
    BC.get ?schedule ({ Build.pull; pool; timeout; level }, fs, proc)
    { Build.Key.commit; dockerfile; docker_context; squash; buildx; build_args; path }

  module RC = Current_cache.Make(Run)

  let run ~docker_context ?pool ?(run_args=[]) ~proc image ~args  =
    RC.get { Run.pool; proc } { Run.Key.image; args; docker_context; run_args }

  module PrC = Current_cache.Make(Pread)

  let pread ~docker_context ?pool ?(run_args=[]) ~proc image ~args =
    PrC.get { Pread.pool; proc } { Pread.Key.image; args; docker_context; run_args }

  module TC = Current_cache.Output(Tag)

  let tag ~docker_context ~tag ~proc image =
    TC.set proc { Tag.Key.tag; docker_context } { Tag.Value.image }

  module Push_cache = Current_cache.Output(Push)

  let push ~docker_context ?auth ~tag ~proc image =
    Push_cache.set (auth, proc) { Push.Key.tag; docker_context } { Push.Value.image }

  module SC = Current_cache.Output(Service)

  let service ~docker_context ~name ~image ~proc () =
    SC.set proc { Service.Key.name; docker_context } { Service.Value.image }

  module CC = Current_cache.Output(Compose)

  let compose ?(pull=true) ~docker_context ~name ~contents ~proc () =
    CC.set Compose.{ pull; proc } { Compose.Key.name; docker_context } { Compose.Value.contents }

  module CCC = Current_cache.Output(Compose_cli)

  let compose_cli ?(pull=true) ?(up_args = []) ~docker_context ~name ~detach ~contents ~proc () =
     CCC.set Compose_cli.{ pull; proc } { Compose_cli.Key.name; docker_context; detach ; up_args } { Compose_cli.Value.contents }

  module Cmd = struct

    let ( >>!= ) = Result.bind

    type t = Current.Process.command

    let docker args ~docker_context = Cmd.docker ~docker_context args

    let rm_f id = docker ["container"; "rm"; "-f"; id]
    let kill id = docker ["container"; "kill"; id]

    (* Try to "docker kill $id". If it fails, just log a warning and continue. *)
    let try_kill_container ~docker_context ~job proc id =
      match Current.Process.exec ~cancellable:false ~job proc (kill ~docker_context id) with
      | Ok () -> ()
      | Error (`Msg m) -> Current.Job.log job "Warning: Failed to kill container %S: %s" id m

    let with_container ~docker_context ~kill_on_cancel ~job t cmd fn =
      Current.Process.check_output ~cancellable:false ~job t cmd >>!= fun id ->
      let id = String.trim id in
      let did_rm = ref false in
      let result =
        try
           begin
             if kill_on_cancel then (
               Current.Job.on_cancel job (fun _ ->
                   if !did_rm = false then try_kill_container ~docker_context ~job t id
                )
             )
           end;
           fn id
        with ex -> (Fmt.error_msg "with_container: uncaught exception: %a" Fmt.exn ex)
      in
      did_rm := true;
      match Current.Process.exec ~cancellable:false ~job t (rm_f ~docker_context id) with
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

module Make (Host : S.HOST) = struct
  module Image = Image

  let docker_context = Host.docker_context

  let pp_opt_arch f = function
    | None -> ()
    | Some arch -> Fmt.pf f "@,%s" arch

  let pull ?auth ?label ?arch ~schedule ~proc tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s%a" label pp_opt_arch arch |>
    let> () = Current.return () in
    Raw.pull ~docker_context ~schedule ?arch ?auth ~proc tag

  let peek ?label ~arch ~schedule ~proc tag =
    let label = Option.value label ~default:tag in
    Current.component "peek %s@,%s" label arch |>
    let> () = Current.return () in
    Raw.peek ~docker_context ~schedule ~arch ~proc tag

  let pp_sp_label = Fmt.(option (sp ++ string))

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit
    | `Dir path -> Current.map (fun path -> `Dir path) path

  let build ?level ?schedule ?timeout ?squash ?buildx ?label ?dockerfile ?path ?pool ?build_args ~pull ~fs ~proc src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    Raw.build ~fs ~docker_context ?level ?schedule ?timeout ?squash ?buildx ?dockerfile ?path ?pool ?build_args ~pull ~proc commit

  let run ?label ?pool ?run_args ~proc image ~args =
    Current.component "run%a" pp_sp_label label |>
    let> image = image in
    Raw.run ~docker_context ?pool ?run_args ~proc image ~args

  let pread ?label ?pool ?run_args ~proc image ~args =
    Current.component "pread%a" pp_sp_label label |>
    let> image = image in
    Raw.pread ~docker_context ?pool ?run_args ~proc image ~args

  let tag ~tag ~proc image =
    Current.component "docker-tag@,%a" pp_tag tag |>
    let> image = image in
    Raw.tag ~docker_context ~tag ~proc image

  let push ?auth ~tag ~proc image =
    Current.component "docker-push@,%a" pp_tag tag |>
    let> image = image in
    Raw.push ~docker_context ?auth ~tag ~proc image

  let service ~name ~image ~proc () =
    Current.component "docker-service@,%s" name |>
    let> image = image in
    Raw.service ~docker_context ~name ~image ~proc ()

  let compose ?pull ~name ~contents ~proc () =
    Current.component "docker-compose@,%s" name |>
    let> contents = contents in
    Raw.compose ?pull ~docker_context ~name ~contents ~proc ()

  let compose_cli ?pull ?up_args ~name ~detach ~contents ~proc () =
    Current.component "docker-compose-cli@,%s" name |>
    let> contents = contents in
    Raw.compose_cli ?pull ?up_args ~docker_context ~name ~detach ~contents ~proc ()
end

module Default = Make(struct
    let docker_context = Sys.getenv_opt "DOCKER_CONTEXT"
  end)

module MC = Current_cache.Output(Push_manifest)

let push_manifest ?auth ~tag ~fs ~proc manifests =
  Current.component "docker-push-manifest@,%a" pp_tag tag |>
  let> manifests = Current.list_seq manifests in
  MC.set (auth, fs, proc) tag { Push_manifest.Value.manifests }
