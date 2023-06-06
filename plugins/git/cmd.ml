let dir_exists d =
  match Bos.OS.Dir.exists d with
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let hexchars = "0123456789abcdef"

let pp_hex f d =
  for x = 0 to Cstruct.length d - 1 do
    let byte = Cstruct.get_uint8 d x in
    Fmt.pf f "%02x" byte
  done

let id_of_repo repo =
  let module Hash = Mirage_crypto.Hash.SHA256 in
  let base = Filename.basename repo in
  let digest = Hash.digest (Cstruct.of_string repo) in
  Fmt.str "%s-%a" base pp_hex digest

(* .../var/git/myrepo-hhh *)
let local_copy repo =
  let repos_dir = Current.state_dir "git" in
  Fpath.append repos_dir (Fpath.v (id_of_repo repo))

let git ~cancellable ~job ?cwd ?config p args =
  let args =
    match cwd with
    | None -> args
    | Some cwd -> "-C" :: Fpath.to_string cwd :: args
  in
  let config = match config with
  | None -> []
  | Some config ->
          List.map (fun config -> ["-c" ; config]) config
          |> List.flatten
  in
  let cmd = "git" :: config @ args in
  Current.Process.exec ~cancellable ~job p ("", cmd)

(*  This command manipulates paths. It requires [protocol.file.allow=always] to
    be set to make sure we can update the submodules.

    Cf: https://git-scm.com/docs/git-config#Documentation/git-config.txt-protocolallow *)
let git_clone ~cancellable ~job ~src p dst =
    let config = [ "protocol.file.allow=always" ] in
    git ~config ~cancellable ~job p ["clone"; "--recursive"; "--verbose"; src; Fpath.to_string dst]

let git_fetch ?recurse_submodules ~cancellable ~job ~src ~dst p gref =
  let flags =
    match recurse_submodules with
    | None -> []
    | Some x -> ["--recurse-submodules=" ^ string_of_bool x]
  in
  git ~cancellable ~job ~cwd:dst p ("fetch" :: flags @ ["-q"; "-f"; src; gref])

let git_reset_hard ~job ~repo p hash =
  git ~cancellable:false ~job ~cwd:repo p ["reset"; "--hard"; "-q"; hash]

let git_remote_set_url ~job ~repo ~remote p url =
  git ~cancellable:false ~job ~cwd:repo p ["remote"; "set-url"; remote; url]

let git_rev_parse ?(cancellable=false) ~job ~repo p x =
  let cmd = ["git"; "-C"; Fpath.to_string repo; "rev-parse"; x] in
  Current.Process.check_output ~cancellable ~job p ("", cmd) |> Stdlib.Result.map String.trim

let cp_r ~cancellable ~job ~src ~dst p =
  let cmd = [ "cp"; "-a"; "--"; Fpath.to_string src; dst ] in
  Current.Process.exec ~cancellable ~job p ("", cmd)

let git_submodule_sync ~cancellable ~job ~repo p =
  git ~cancellable ~job ~cwd:repo p ["submodule"; "sync"]

let git_submodule_deinit ~cancellable ~job ~repo ~force ~all p =
  let flags = List.concat [
      (if force then ["--force"] else []);
      (if all then ["--all"] else []);
    ]
  in
  git ~cancellable ~job ~cwd:repo p ("submodule" :: "deinit" :: flags)

(*  This command manipulates paths. It requires [protocol.file.allow=always] to
    be set to make sure we can update the submodules.

    cf: https://git-scm.com/docs/git-config#Documentation/git-config.txt-protocolallow *)
let git_submodule_update ~cancellable ~job ~repo ~init ~fetch p =
  let config = [ "protocol.file.allow=always" ] in
  let flags = List.concat [
      (if init then ["--init"] else []);
      (if fetch then [] else ["--no-fetch"]);
    ]
  in
  git ~config ~cancellable ~job ~cwd:repo p ("submodule" :: "update" :: "--recursive" :: flags)
