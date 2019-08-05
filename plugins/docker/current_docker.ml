open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(unit ":@,") string)

module Make (Host : S.HOST) = struct
  module Image = Image

  module PC = Current_cache.Make(Pull)

  let docker_host = Host.docker_host

  let pull ?label ~schedule tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s" label |>
    let> () = Current.return () in
    PC.get ~schedule Pull.No_context { Pull.Key.docker_host; tag }

  module BC = Current_cache.Make(Build)

  let pp_sp_label = Fmt.(option (prefix sp string))

  let option_map f = function
    | None -> None
    | Some x -> Some (f x)

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit

  let build ?schedule ?(squash=false) ?label ?dockerfile ~pull src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    let dockerfile = option_map Dockerfile.string_of_t dockerfile in
    BC.get ?schedule { Build.pull } { Build.Key.commit; dockerfile; docker_host; squash }

  module RC = Current_cache.Make(Run)

  let run image ~args =
    Current.component "run" |>
    let> image = image in
    RC.get Run.No_context { Run.Key.image; args; docker_host }

  module TC = Current_cache.Output(Tag)

  let tag ~tag image =
    Current.component "docker-tag@ %a" pp_tag tag |>
    let> image = image in
    TC.set None { Tag.Key.tag; docker_host } { Tag.Value.image; op = `Tag }

  let push ?auth ~tag image =
    Current.component "docker-push@ %a" pp_tag tag |>
    let> image = image in
    TC.set auth { Tag.Key.tag; docker_host } { Tag.Value.image; op = `Push }

  module SC = Current_cache.Output(Service)

  let service ~name ~image () =
    Current.component "docker-service@ %s" name |>
    let> image = image in
    SC.set Service.No_context { Service.Key.name; docker_host } { Service.Value.image }
end

module Default = Make(struct
    let docker_host = Sys.getenv_opt "DOCKER_HOST"
  end)

module MC = Current_cache.Output(Push_manifest)

let push_manifest ?auth ~tag manifests =
  Current.component "docker-push-manifest@ %a" pp_tag tag |>
  let> manifests = Current.list_seq manifests in
  MC.set auth tag { Push_manifest.Value.manifests }
