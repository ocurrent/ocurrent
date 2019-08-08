let host_args = function
  | None -> []
  | Some context -> ["--context"; context]

let docker ~docker_context args =
  "", Array.of_list ("docker" :: host_args docker_context @ args)

let login ~docker_context ~user =
  docker ~docker_context ["login"; "--password-stdin"; "--username"; user]

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
