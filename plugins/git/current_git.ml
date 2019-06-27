open Current.Syntax
open Lwt.Infix

let src = Logs.Src.create "current.git" ~doc:"OCurrent git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module Commit_id = Commit_id
module Commit = Commit

let ( >>!= ) x f =
  x >>= function
  | Ok y -> f y
  | Error _ as e -> Lwt.return e

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

module Fetch = struct
  type t = No_context
  module Key = Commit_id
  module Value = Commit

  let id = "git-fetch"

  let build ~switch:_ No_context job key =
    let { Commit_id.repo = remote_repo; gref; hash = _ } = key in
    let local_repo = local_copy remote_repo in
    (* Ensure we have a local clone of the repository. *)
    begin
      if dir_exists local_repo then Lwt.return (Ok ())
      else Cmd.git_clone ~job ~src:remote_repo local_repo
    end >>!= fun () ->
    let commit = { Commit.repo = local_repo; id = key } in
    (* Fetch the commit (if missing). *)
    begin
      Commit.check_cached ~job commit >>= function
      | Ok () -> Lwt.return (Ok ())
      | Error _ -> Cmd.git_fetch ~job ~src:remote_repo ~dst:local_repo gref
    end >>!= fun () ->
    (* Check we got the commit we wanted. *)
    Commit.check_cached ~job commit >>!= fun () ->
    Lwt.return @@ Ok commit

  let pp f key = Fmt.pf f "git fetch %a" Key.pp key

  let auto_cancel = false

  let level _ k =
    if Commit_id.is_local k then Current.Level.Harmless
    else Current.Level.Mostly_harmless
end

module Fetch_cache = Current_cache.Make(Fetch)

let fetch cid =
  "fetch" |>
  let** cid = cid in
  Fetch_cache.get Fetch.No_context cid

let with_checkout ~job commit fn =
  let { Commit.repo; id } = commit in
  Current_cache.Process.with_tmpdir ~prefix:"git-checkout" @@ fun tmpdir ->
  Cmd.git_clone ~job ~src:(Fpath.to_string repo) tmpdir >>!= fun () ->
  Cmd.git_reset_hard ~job ~repo:tmpdir id.Commit_id.hash >>= function
  | Ok () -> fn tmpdir
  | Error e ->
    Commit.check_cached ~job commit >>= function
    | Error not_cached ->
      Fetch_cache.invalidate id;
      Lwt.return (Error not_cached)
    | Ok () -> Lwt.return (Error e)

module Local = struct
  module Ref_map = Map.Make(String)

  let next_id =
    let i = ref 0 in
    fun () ->
      let id = !i in
      incr i;
      id

  type t = {
    repo : Fpath.t;
    head : [`Ref of string | `Commit of Commit_id.t] Current.Input.t;
    mutable heads : Commit.t Current.Input.t Ref_map.t;
  }

  let pp_repo f t = Fpath.pp f t.repo

  let read_reference t gref =
    let cmd = [| "git"; "-C"; Fpath.to_string t.repo; "rev-parse"; "--revs-only"; gref |] in
    Lwt_process.pread ("", cmd) >|= fun out ->
    match String.trim out with
    | "" -> Error (`Msg (Fmt.strf "Unknown ref %S" gref))
    | hash ->
      let id = { Commit_id.repo = Fpath.to_string t.repo; gref; hash } in
      Ok { Commit.repo = t.repo; id }

  let make_input t gref =
    let dot_git = Fpath.(t.repo / ".git") in
    if not (Astring.String.is_prefix ~affix:"refs/" gref) then
      Fmt.failwith "Reference %S should start \"refs/\"" gref;
    let read () = read_reference t gref in
    let watch refresh =
      let watch_dir = Fpath.append dot_git (Fpath.v @@ Filename.dirname gref) in
      Log.info (fun f -> f "Installing watch for %a" Fpath.pp watch_dir);
      Irmin_watcher.hook (next_id ()) (Fpath.to_string watch_dir) (fun path ->
          if path = Filename.basename gref then (
            Log.info (fun f -> f "Detected change in %S" path);
            refresh ();
          ) else (
            Log.debug (fun f -> f "Ignoring change in %S" path);
          );
          Lwt.return_unit
        )
      >|= fun unwatch ->
      Log.info (fun f -> f "Watch installed for %a" Fpath.pp watch_dir);
      fun () ->
        Log.info (fun f -> f "Unwatching %a" Fpath.pp watch_dir);
        unwatch ()
    in
    let pp f =
      Fmt.pf f "%a#%s" pp_repo t gref
    in
    Current.monitor ~read ~watch ~pp

  let commit_of_ref t gref =
    let i =
      match Ref_map.find_opt gref t.heads with
      | Some i -> i
      | None ->
        let i = make_input t gref in
        t.heads <- Ref_map.add gref i t.heads;
        i
    in
    Current.track i

  let head t = Current.track t.head

  let head_commit t =
    "head commit" |>
    let** h = head t in
    match h with
    | `Commit id -> Current.return { Commit.repo = t.repo; id }
    | `Ref gref -> commit_of_ref t gref

  let read_head repo =
    let path = Fpath.(repo / ".git" / "HEAD") in
    match Bos.OS.File.read path with
    | Error _ as e -> e
    | Ok contents ->
      let open Astring in (* (from ocaml-git) *)
      match String.cuts ~sep:" " (String.trim contents) with
      | [hash] -> Ok (`Commit {Commit_id.repo = Fpath.to_string repo; gref = "HEAD"; hash})
      | [_;r]  -> Ok (`Ref r)
      | _      -> Error (`Msg (Fmt.strf "Can't parse HEAD %S" contents))

  let make_head_input repo =
    let dot_git = Fpath.(repo / ".git") in
    let read () = Lwt.return (read_head repo) in
    let watch refresh =
      let watch_dir = dot_git in
      Log.info (fun f -> f "Installing watch for %a" Fpath.pp watch_dir);
      Irmin_watcher.hook (next_id ()) (Fpath.to_string watch_dir) (fun path ->
          if path = "HEAD" then (
            Log.info (fun f -> f "Detected change in %S" path);
            refresh ();
          ) else (
            Log.debug (fun f -> f "Ignoring change in %S" path);
          );
          Lwt.return_unit
        )
      >|= fun unwatch ->
      Log.info (fun f -> f "Watch installed for %a" Fpath.pp watch_dir);
      fun () ->
        Log.info (fun f -> f "Unwatching %a" Fpath.pp watch_dir);
        unwatch ()
    in
    let pp f =
      Fmt.pf f "HEAD(%a)" Fpath.pp repo
    in
    Current.monitor ~read ~watch ~pp

  let v repo =
    let repo = Fpath.normalize @@ Fpath.append (Fpath.v (Sys.getcwd ())) repo in
    let head = make_head_input repo in
    let heads = Ref_map.empty in
    { repo; head; heads }
end
