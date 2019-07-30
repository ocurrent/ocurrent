let host_args = function
  | None -> []
  | Some host -> ["-H"; host]

let docker ~docker_host args =
  "", Array.of_list ("docker" :: host_args docker_host @ args)

let login ~docker_host ~user =
  docker ~docker_host ["login"; "--password-stdin"; "--username"; user]

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
