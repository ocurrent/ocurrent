open Current.Syntax

let src = Logs.Src.create "current.git" ~doc:"OCurrent git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module Commit_id = Commit_id
module Commit = Commit

let ( >>!= ) x f =
  match x with
  | Ok y -> f y
  | Error _ as e -> e

module Fetch = struct
  type t = Eio.Process.mgr
  module Key = Commit_id
  module Value = Commit

  let id = "git-fetch"

  let build proc job key =
    let { Commit_id.repo = remote_repo; gref; hash = _ } = key in
    let level =
      if Commit_id.is_local key then Current.Level.Harmless
      else Current.Level.Mostly_harmless
    in
    Current.Job.start job ~level;
    Eio.Mutex.use_ro (Clone.repo_lock remote_repo) @@ fun () ->
    Current.Job.write job "Fetching...\n";
    let local_repo = Cmd.local_copy remote_repo in
    (* Ensure we have a local clone of the repository. *)
    begin
      if Cmd.dir_exists local_repo then Ok ()
      else Cmd.git_clone ~cancellable:true ~job ~src:remote_repo proc local_repo
    end >>!= fun () ->
    Current.Job.write job "Past clone...\n";
    let commit = { Commit.repo = local_repo; id = key } in
    (* Fetch the commit (if missing). *)
    begin
      match Commit.check_cached ~cancellable:false ~job commit proc with
      | Ok () -> Ok ()
      | Error _ -> Cmd.git_fetch ~cancellable:true ~job ~recurse_submodules:false ~src:remote_repo ~dst:local_repo proc gref
    end >>!= fun () ->
    (* Check we got the commit we wanted. *)
    Commit.check_cached ~cancellable:false ~job commit proc >>!= fun () ->
    (* Get any submodules too.
       "sync" is needed to handle repositories moving.
       "deinit" is needed to handle submodules being removed.
       Note:
       This could cause trouble if we cancel during the "submodule update --init" step, leaving some
       submodules uninitialised. Then, the "submodule sync" for the next fetch won't do anything
       and, if the submodule has moved, updating will fail. However, it should eventually recover as
       the init step happens before the fetch so we'll make a little progress while failing. This is
       rare enough that it's probably not worth trying to detect this and implementing a retry loop
       here. What we really want is "submodule update --init --sync --recursive --prune", but Git
       doesn't offer that. *)
    Cmd.git_reset_hard ~job ~repo:local_repo proc commit.id.hash >>!= fun () ->
    Cmd.git_submodule_sync ~cancellable:false ~job ~repo:local_repo proc >>!= fun () ->
    Cmd.git_submodule_deinit ~force:true ~all:true ~cancellable:false ~job ~repo:local_repo proc >>!= fun () ->
    Cmd.git_submodule_update ~init:true ~cancellable:true ~fetch:true ~job ~repo:local_repo proc >>!= fun () ->
    Ok commit

  let pp f key = Fmt.pf f "git fetch %a" Key.pp key

  let auto_cancel = false
end

module Fetch_cache = Current_cache.Make(Fetch)

let fetch proc cid =
  Current.component "fetch" |>
  let> cid = cid in
  Fetch_cache.get proc cid

module Clone_cache = Current_cache.Make(Clone)

let clone ~schedule ?(gref="master") proc repo =
  Current.component "clone@ %s@ %s" repo gref |>
  let> () = Current.return () in
  Clone_cache.get ~schedule proc { Clone.Key.repo; gref }

let with_checkout ?pool ~job ~fs proc commit fn =
  let { Commit.repo; id } = commit in
  let short_hash = Astring.String.with_range ~len:8 id.Commit_id.hash in
  Current.Job.log job "@[<v2>Checking out commit %s. To reproduce:@,%a@]"
    short_hash Commit_id.pp_user_clone id;
  Fun.protect
    (fun () ->
       Eio.Promise.await_exn
       begin
         match pool with
         | Some pool -> Current.Job.use_pool job pool
         | None -> Eio.Promise.create_resolved (Ok ())
       end;
       Current.Process.with_tmpdir ~prefix:"git-checkout" fs @@ fun tmpdir ->
       Cmd.cp_r ~cancellable:true ~job ~src:(Fpath.(repo / ".git")) ~dst:(snd tmpdir) proc >>!= fun () ->
       let ftmpdir = Fpath.v (snd tmpdir) in
       Cmd.git_submodule_deinit ~force:true ~all:true ~cancellable:false ~job ~repo:ftmpdir proc >>!= fun () ->
       match Cmd.git_reset_hard ~job ~repo:ftmpdir proc id.Commit_id.hash with
       | Ok () ->
         Cmd.git_submodule_update ~init:true ~cancellable:true ~fetch:false ~job ~repo:ftmpdir proc >>!= fun () ->
         (* Eio.Promise.await @@ Current.Switch.turn_off switch; *)
         fn tmpdir
       | Error e ->
         match Commit.check_cached ~cancellable:false ~job commit proc with
         | Error not_cached ->
           Fetch_cache.invalidate id;
           Error not_cached
         | Ok () -> Error e
    )
    ~finally:(fun () -> ())

module Local = struct
  module Ref_map = Map.Make(String)

  let next_id =
    let i = ref 0 in
    fun () ->
      let id = !i in
      incr i;
      id

  type t = {
    proc : Eio.Process.mgr;
    repo : Fpath.t;
    head : [`Ref of string | `Commit of Commit_id.t] Current.Monitor.t;
    mutable heads : Commit.t Current.Monitor.t Ref_map.t;
  }

  let pp_repo f t = Fpath.pp f t.repo

  let pread ~proc _executable args = Eio.Process.parse_out proc Eio.Buf_read.take_all args

  let read_reference t gref =
    let out =
      let args = [ "git"; "-C"; Fpath.to_string t.repo; "rev-parse"; "--revs-only"; gref ] in
      let out = pread ~proc:t.proc "git" args in
      match String.trim out with
      | "" -> Error (`Msg (Fmt.str "Unknown ref %S" gref))
      | hash ->
        let id = { Commit_id.repo = Fpath.to_string t.repo; gref; hash } in
        Ok { Commit.repo = t.repo; id }
    in
    Eio.Promise.create_resolved out

  let make_monitor t gref =
    let dot_git = Fpath.(t.repo / ".git") in
    if not (Astring.String.is_prefix ~affix:"refs/" gref) then
      Fmt.failwith "Reference %S should start \"refs/\"" gref;
    let read () = read_reference t gref in
    let watch (refresh : unit -> unit) =
      let watch_dir = Fpath.append dot_git (Fpath.v @@ Filename.dirname gref) in
      Log.debug (fun f -> f "Installing watch for %a" Fpath.pp watch_dir);
      let unwatch =
        Irmin_watcher.hook (next_id ()) (Fpath.to_string watch_dir) (fun path ->
          if path = Filename.basename gref then (
            Log.info (fun f -> f "Detected change in %S" path);
            refresh ();
          ) else (
            Log.debug (fun f -> f "Ignoring change in %S" path);
          );
        )
      in
      Log.debug (fun f -> f "Watch installed for %a" Fpath.pp watch_dir);
      fun () ->
        Log.debug (fun f -> f "Unwatching %a" Fpath.pp watch_dir);
        unwatch ()
    in
    let pp f =
      Fmt.pf f "%a#%s" pp_repo t gref
    in
    Current.Monitor.create ~read ~watch ~pp

  let commit_of_ref t gref =
    match Ref_map.find_opt gref t.heads with
    | Some i -> i
    | None ->
      let i = make_monitor ~sw:(Current.Engine.switch ()) t gref in
      t.heads <- Ref_map.add gref i t.heads;
      i

  let repo t =
    t.repo

  let head t =
    Current.component "head" |>
    let> () = Current.return () in
    Current.Monitor.get t.head

  let head_commit t =
    Current.component "head commit" |>
    let> h = head t in
    match h with
    | `Commit id -> Current.Primitive.const { Commit.repo = t.repo; id }
    | `Ref gref -> Current.Monitor.get @@ commit_of_ref t gref

  let commit_of_ref t gref =
    Current.component "commit_of_ref %s" gref |>
    let> () = Current.return () in
    Current.Monitor.get @@ commit_of_ref t gref

  let read_head repo =
    let path = Fpath.(repo / ".git" / "HEAD") in
    match Bos.OS.File.read path with
    | Error _ as e -> e
    | Ok contents ->
      let open Astring in (* (from ocaml-git) *)
      match String.cuts ~sep:" " (String.trim contents) with
      | [hash] -> Ok (`Commit {Commit_id.repo = Fpath.to_string repo; gref = "HEAD"; hash})
      | [_;r]  -> Ok (`Ref r)
      | _      -> Error (`Msg (Fmt.str "Can't parse HEAD %S" contents))

  let make_head repo =
    let dot_git = Fpath.(repo / ".git") in
    let read () = Eio.Promise.create_resolved @@ read_head repo in
    let watch refresh =
      let watch_dir = dot_git in
      Log.debug (fun f -> f "Installing watch for %a" Fpath.pp watch_dir);
      let unwatch =
        Irmin_watcher.hook (next_id ()) (Fpath.to_string watch_dir) (fun path ->
            if path = "HEAD" then (
              Log.info (fun f -> f "Detected change in %S" path);
              refresh ();
            ) else (
              Log.debug (fun f -> f "Ignoring change in %S" path);
            )
          )
      in
      Log.debug (fun f -> f "Watch installed for %a" Fpath.pp watch_dir);
      fun () ->
        Log.debug (fun f -> f "Unwatching %a" Fpath.pp watch_dir);
        unwatch ()
    in
    let pp f =
      Fmt.pf f "HEAD(%a)" Fpath.pp repo
    in
    Current.Monitor.create ~sw:(Current.Engine.switch ()) ~read ~watch ~pp

  let v proc repo =
    let repo = Fpath.normalize @@ Fpath.append (Fpath.v (Sys.getcwd ())) repo in
    let head = make_head repo in
    let heads = Ref_map.empty in
    { proc; repo; head; heads }
end
