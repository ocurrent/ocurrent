let git ?switch ~job ?cwd args =
  let args =
    match cwd with
    | None -> args
    | Some cwd -> "-C" :: Fpath.to_string cwd :: args
  in
  let cmd = Array.of_list ("git" :: args) in
  Current.Process.exec ?switch ~job ("", cmd)

let git_clone ~switch ~job ~src dst =
  git ~switch ~job ["clone"; "-q"; src; Fpath.to_string dst]

let git_fetch ~switch ~job ~src ~dst gref =
  git ~switch ~job ~cwd:dst ["fetch"; src; gref]

let git_reset_hard ~job ~repo hash =
  git ~job ~cwd:repo ["reset"; "--hard"; hash]

let git_remote_set_url ~job ~repo ~remote url =
  git ~job ~cwd:repo ["remote"; "set-url"; remote; url]
