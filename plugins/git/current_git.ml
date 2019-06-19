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

  let is_local t =
    if Astring.String.is_prefix ~affix:"file:" t.repo then true
    else match String.index_opt t.repo ':',
               String.index_opt t.repo '/' with
    | Some i, Some j -> i < j     (* http://... is remote; /http:foo is local *)
    | None, _ -> true             (* All remote URLs have colons *)
    | Some _, None -> false       (* foo:bar is remote *)

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

  let level _ k =
    if Commit_id.is_local k then Current.Level.Harmless
    else Current.Level.Mostly_harmless
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
    mutable heads : Commit_id.t Current.Input.t Ref_map.t;
  }

  let pp_repo f t = Fpath.pp f t.repo

  let read_reference t gref =
    let cmd = [| "git"; "-C"; Fpath.to_string t.repo; "rev-parse"; "--revs-only"; gref |] in
    Lwt_process.pread ("", cmd) >|= fun out ->
    match String.trim out with
    | "" -> Error (`Msg (Fmt.strf "Unknown ref %S" gref))
    | hash -> Ok { Commit_id.repo = Fpath.to_string t.repo; gref; hash }

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

  let commit_of_ref t head_ref =
    "read gref" |>
    let** x = head_ref in
    match x with
    | `Commit c -> Current.return c
    | `Ref gref ->
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
