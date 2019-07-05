open Current.Syntax

type source = Current_git.Commit.t

module S = S

module Make (Host : S.HOST) = struct
  module Image = Image

  module PC = Current_cache.Make(Pull)

  let docker_host = Host.docker_host

  let pull ~schedule tag =
    Current.component "pull %s" tag |>
    let** () = Current.return () in
    PC.get ~schedule Pull.No_context { Pull.Key.docker_host; tag }

  module BC = Current_cache.Make(Build)

  let pp_sp_label = Fmt.(option (prefix sp string))

  let option_map f = function
    | None -> None
    | Some x -> Some (f x)

  let build ?label ?dockerfile ~pull src =
    Current.component "build%a" pp_sp_label label |>
    let** commit = src
    and* dockerfile = Current.option_seq dockerfile in
    let dockerfile = option_map Dockerfile.string_of_t dockerfile in
    BC.get { Build.pull } { Build.Key.commit; dockerfile; docker_host }

  module RC = Current_cache.Make(Run)

  let run image ~args =
    Current.component "run" |>
    let** image = image in
    RC.get Run.No_context { Run.Key.image; args; docker_host }

  module TC = Current_cache.Output(Tag)

  let tag ~tag image =
    Current.component "docker-tag %s" tag |>
    let** image = image in
    TC.set Tag.No_context { Tag.Key.tag; docker_host } { Tag.Value.image; op = `Tag }

  let push ~tag image =
    Current.component "docker-push %s" tag |>
    let** image = image in
    TC.set Tag.No_context { Tag.Key.tag; docker_host } { Tag.Value.image; op = `Push }
end

module Default = Make(struct
    let docker_host = Sys.getenv_opt "DOCKER_HOST"
  end)
