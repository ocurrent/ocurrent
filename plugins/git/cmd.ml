let git ?switch ~job ?cwd args =
  let args =
    match cwd with
    | None -> args
    | Some cwd -> "-C" :: Fpath.to_string cwd :: args
  in
  let cmd = Array.of_list ("git" :: args) in
  Current_cache.Process.exec ?switch ~job ("", cmd)

let git_clone ~switch ~job ~src dst =
  git ~switch ~job ["clone"; src; Fpath.to_string dst]

let git_fetch ~switch ~job ~src ~dst gref =
  git ~switch ~job ~cwd:dst ["fetch"; src; gref]

let git_reset_hard ~job ~repo hash =
  git ~job ~cwd:repo ["reset"; "--hard"; hash]
