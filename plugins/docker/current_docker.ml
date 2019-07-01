open Current.Syntax

type source = Current_git.Commit.t
module Image = Image

module PC = Current_cache.Make(Pull)

let pull ~schedule tag =
  Fmt.strf "pull %s" tag |>
  let** () = Current.return () in
  PC.get ~schedule Pull.No_context tag

module BC = Current_cache.Make(Build)

let pp_sp_label = Fmt.(option (prefix sp string))

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let build ?label ?dockerfile ~pull src =
  Fmt.strf "build%a" pp_sp_label label |>
  let** commit = src
  and* dockerfile = Current.option_seq dockerfile in
  let dockerfile = option_map Dockerfile.string_of_t dockerfile in
  BC.get { Build.pull } { Build.Key.commit; dockerfile }

module RC = Current_cache.Make(Run)

let run image ~args =
  "run" |>
  let** image = image in
  RC.get Run.No_context (image, args)
