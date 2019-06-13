open Current.Syntax
open Lwt.Infix

let src = Logs.Src.create "current.git" ~doc:"OCurrent git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >>!= ) x f =
  x >>= function
  | Ok y -> f y
  | Error _ as e -> Lwt.return e

module Commit_id = struct
  type t = {
    repo : string;  (* Remote repository from which to pull. *)
    gref : string;  (* Ref to pull, e.g. "master" or "pull/12/head". *)
    hash : string;  (* Hash that [gref] is expected to have. *)
  }

  let v ~repo ~gref ~hash = { repo; gref; hash }

  let pp f {repo; gref; hash} =
    Fmt.pf f "%s#%s (%s)" repo gref hash

  let equal = (=)
  let compare = compare
end

let git ?cwd args =
  let args =
    match cwd with
    | None -> args
    | Some cwd -> "-C" :: Fpath.to_string cwd :: args
  in
  let cmd = Array.of_list ("git" :: args) in
  Process.exec ("", cmd)

let git_clone ~src dst =
  git ["clone"; src; Fpath.to_string dst]

let git_fetch ~src ~dst gref =
  git ~cwd:dst ["fetch"; src; gref]

let git_reset_hard ~repo hash =
  git ~cwd:repo ["reset"; "--hard"; hash]

module Commit = struct
  type t = {
    repo : Fpath.t;
    id : Commit_id.t;
  }

  let id t = t.id.Commit_id.hash
  let compare a b = String.compare (id a) (id b)
  let equal a b = String.equal (id a) (id b)
  let pp = Fmt.using id Fmt.string

  let check_cached t =
    let hash = id t in
    let branch = Fmt.strf "fetch-%s" hash in
    git ~cwd:t.repo ["branch"; "-f"; branch; hash]
end

let id_of_repo repo =
  let base = Filename.basename repo in
  let digest = Digest.string repo |> Digest.to_hex in
  Fmt.strf "%s-%s" base digest

(* .../var/git/myrepo-hhh *)
let local_copy repo =
  let repos_dir = Current.state_dir "git" in
  Fpath.append repos_dir (Fpath.v (id_of_repo repo))

let dir_exists d =
  match Bos.OS.Dir.exists d with
  | Ok x -> x
  | Error (`Msg x) -> failwith x

module Fetcher = struct
  type t = No_context
  module Key = Commit_id
  module Value = Commit

  let build ~switch:_ No_context key =
    let { Commit_id.repo = remote_repo; gref; hash = _ } = key in
    let local_repo = local_copy remote_repo in
    (* Ensure we have a local clone of the repository. *)
    begin
      if dir_exists local_repo then Lwt.return (Ok ())
      else git_clone ~src:remote_repo local_repo
    end >>!= fun () ->
    let commit = { Commit.repo = local_repo; id = key } in
    (* Fetch the commit (if missing). *)
    begin
      Commit.check_cached commit >>= function
      | Ok () -> Lwt.return (Ok ())
      | Error _ -> git_fetch ~src:remote_repo ~dst:local_repo gref
    end >>!= fun () ->
    (* Check we got the commit we wanted. *)
    Commit.check_cached commit >>!= fun () ->
    Lwt.return @@ Ok commit

  let pp f key = Fmt.pf f "git fetch %a" Key.pp key

  let auto_cancel = false
end

module Fetch_cache = Current_cache.Make(Fetcher)

let fetch cid =
  "fetch" |>
  let** cid = cid in
  Fetch_cache.get Fetcher.No_context cid

let with_checkout commit fn =
  let { Commit.repo; id } = commit in
  Process.with_tmpdir ~prefix:"git-checkout" ~mode:0o700 @@ fun tmpdir ->
  git_clone ~src:(Fpath.to_string repo) tmpdir >>!= fun () ->
  git_reset_hard ~repo:tmpdir id.Commit_id.hash >>= function
  | Ok () -> fn tmpdir
  | Error e ->
    Commit.check_cached commit >>= function
    | Error not_cached ->
      Fetch_cache.invalidate id;
      Lwt.return (Error not_cached)
    | Ok () -> Lwt.return (Error e)
