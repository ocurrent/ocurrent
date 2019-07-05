let host_args = function
  | None -> []
  | Some host -> ["-H"; host]

let docker ~docker_host args =
  "", Array.of_list ("docker" :: host_args docker_host @ args)

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(Dump.list (quote string)) f (Array.to_list args)
