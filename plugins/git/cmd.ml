let git ~job ?cwd args =
  let args =
    match cwd with
    | None -> args
    | Some cwd -> "-C" :: Fpath.to_string cwd :: args
  in
  let cmd = Array.of_list ("git" :: args) in
  Current_cache.Process.exec ~job ("", cmd)

let git_clone ~job ~src dst =
  git ~job ["clone"; src; Fpath.to_string dst]

let git_fetch ~job ~src ~dst gref =
  git ~job ~cwd:dst ["fetch"; src; gref]

let git_reset_hard ~job ~repo hash =
  git ~job ~cwd:repo ["reset"; "--hard"; hash]
