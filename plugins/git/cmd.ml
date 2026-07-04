open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "git"

  let download_events =
    let help = "Incoming download events" in
    Counter.v_label ~label_name:"event" ~help ~namespace ~subsystem "download_events"
end

let dir_exists d =
  match Bos.OS.Dir.exists d with
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let hexchars = "0123456789abcdef"

let pp_hex f d =
  String.iter (fun c -> Fmt.pf f "%02x" (Char.code c)) d

let id_of_repo repo =
  let base = Filename.basename repo in
  let digest = Digestif.SHA256.(digest_string repo |> to_raw_string) in
  Fmt.str "%s-%a" base pp_hex digest

(* .../var/git/myrepo-hhh *)
let local_copy repo =
  let repos_dir = Current.state_dir "git" in
  Fpath.append repos_dir (Fpath.v (id_of_repo repo))

let git ~cancellable ~job ?cwd ?env ?config args =
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
  let cmd = Array.of_list ("git" :: config @ args) in
  Current.Process.exec ~cancellable ?env ~job ("", cmd)

(*  This command manipulates paths. It requires [protocol.file.allow=always] to
    be set to make sure we can update the submodules.

    Cf: https://git-scm.com/docs/git-config#Documentation/git-config.txt-protocolallow *)
let git_clone ~cancellable ~job ?env ~src dst =
    Prometheus.Counter.inc_one (Metrics.download_events "clone");
    let config = [ "protocol.file.allow=always" ] in
    git ~config ~cancellable ?env ~job ["clone"; "--recursive"; "-q"; src; Fpath.to_string dst]

let git_fetch ?recurse_submodules ~cancellable ~job ?env ~src ~dst gref =
  Prometheus.Counter.inc_one (Metrics.download_events "fetch");
  let flags =
    match recurse_submodules with
    | None -> []
    | Some x -> ["--recurse-submodules=" ^ string_of_bool x]
  in
  git ~cancellable ?env ~job ~cwd:dst ("fetch" :: flags @ ["-q"; "-f"; src; gref])

let git_reset_hard ~job ~repo hash =
  git ~cancellable:false ~job ~cwd:repo ["reset"; "--hard"; "-q"; hash]

let git_remote_set_url ~job ~repo ~remote url =
  git ~cancellable:false ~job ~cwd:repo ["remote"; "set-url"; remote; url]

let git_rev_parse ?(cancellable=false) ~job ~repo x =
  let cmd = ["git"; "-C"; Fpath.to_string repo; "rev-parse"; x] in
  Current.Process.check_output ~cancellable ~job ("", Array.of_list cmd) >|= Stdlib.Result.map String.trim

let cp_r ~cancellable ~job ~src ~dst =
  let cmd = [| "cp"; "-a"; "--"; Fpath.to_string src; Fpath.to_string dst |] in
  Current.Process.exec ~cancellable ~job ("", cmd)

let git_submodule_sync ~cancellable ~job ~repo =
  git ~cancellable ~job ~cwd:repo ["submodule"; "sync"]

let git_submodule_deinit ~cancellable ~job ~repo ~force ~all =
  let flags = List.concat [
      (if force then ["--force"] else []);
      (if all then ["--all"] else []);
    ]
  in
  git ~cancellable ~job ~cwd:repo ("submodule" :: "deinit" :: flags)

(*  This command manipulates paths. It requires [protocol.file.allow=always] to
    be set to make sure we can update the submodules.

    cf: https://git-scm.com/docs/git-config#Documentation/git-config.txt-protocolallow *)
let git_submodule_update ~cancellable ~job ~repo ~init ~fetch =
  Prometheus.Counter.inc_one (Metrics.download_events "submodule update");
  let config = [ "protocol.file.allow=always" ] in
  let flags = List.concat [
      (if init then ["--init"] else []);
      (if fetch then [] else ["--no-fetch"]);
    ]
  in
  git ~config ~cancellable ~job ~cwd:repo ("submodule" :: "update" :: "--recursive" :: flags)
