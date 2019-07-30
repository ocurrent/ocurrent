open Lwt.Infix

let dir_exists d =
  match Bos.OS.Dir.exists d with
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let id_of_repo repo =
  let base = Filename.basename repo in
  let digest = Digest.string repo |> Digest.to_hex in
  Fmt.strf "%s-%s" base digest

(* .../var/git/myrepo-hhh *)
let local_copy repo =
  let repos_dir = Current.state_dir "git" in
  Fpath.append repos_dir (Fpath.v (id_of_repo repo))

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
  git ~switch ~job ~cwd:dst ["fetch"; "-f"; src; gref]

let git_reset_hard ~job ~repo hash =
  git ~job ~cwd:repo ["reset"; "--hard"; hash]

let git_remote_set_url ~job ~repo ~remote url =
  git ~job ~cwd:repo ["remote"; "set-url"; remote; url]

let git_rev_parse ?switch ~job ~repo x =
  let cmd = ["git"; "-C"; Fpath.to_string repo; "rev-parse"; x] in
  Current.Process.check_output ?switch ~job ("", Array.of_list cmd) >|= Stdlib.Result.map String.trim
