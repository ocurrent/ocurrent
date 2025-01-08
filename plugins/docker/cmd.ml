let context_args = function
  | None -> []
  | Some context -> ["--context"; context]

let config_args = function
  | None -> []
  | Some config -> ["--config"; Fpath.to_string config]

let docker ?config ~docker_context args =
  "", Array.of_list ("docker" :: config_args config @ context_args docker_context @ args)

let compose ~docker_context args =
  "", Array.of_list ("docker-compose" :: context_args docker_context @ args)

let login ?config ?server ~docker_context user =
  docker ?config ~docker_context (["login"; "--password-stdin"; "--username"; user] @ (Option.to_list server))

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
