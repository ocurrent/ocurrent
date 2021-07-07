open Lwt.Infix

type t = No_context

let ( >>!= ) = Lwt_result.bind

module Key = struct
  type t = {
    repo : string;  (* Remote repository from which to pull. *)
    gref : string option;
  } [@@deriving to_yojson]

  let pp f t = Yojson.Safe.pretty_print f (to_yojson t)

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Commit

module Repo_map = Map.Make(String)

let repo_locks = ref Repo_map.empty

let repo_lock repo =
  match Repo_map.find_opt repo !repo_locks with
  | Some l -> l
  | None ->
    let l = Lwt_mutex.create () in
    repo_locks := Repo_map.add repo l !repo_locks;
    l

let id = "git-clone"

let parse_ls_remote_output ~job output =
  match List.find_opt (fun line -> String.sub line 0 4 = "ref:") output with
  | Some ref_line ->
    let split = Astring.String.fields ~empty:false ~is_sep:(function | ' ' | '\t' | '/' -> true | _ -> false) ref_line in
    let branch,_ = List.fold_left (fun (_,b) c -> (b,c)) ("","") split in
    Current.Job.log job "Default branch is '%s'" branch;
    Ok branch
  | None -> Error (`Msg "Failed to parse ls-remote output.")

let build No_context job { Key.repo; gref } =
  Lwt_mutex.with_lock (repo_lock repo) @@ fun () ->
  Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
  let local_repo = Cmd.local_copy repo in
  (match gref with
    | Some v -> Lwt.return_ok v
    | None -> Cmd.git_ls_remote ~job repo >|= (fun res -> Result.bind res (parse_ls_remote_output ~job)))
  >>!= fun gref ->
  (* Ensure we have a local clone of the repository. *)
  begin
    if Cmd.dir_exists local_repo
    then Cmd.git_fetch ~cancellable:true ~job ~src:repo ~dst:local_repo (Fmt.str "%s:refs/remotes/origin/%s" gref gref)
    else Cmd.git_clone ~cancellable:true ~job ~src:repo local_repo
  end >>!= fun () ->
  Cmd.git_rev_parse ~cancellable:true ~job ~repo:local_repo ("origin/" ^ gref) >>!= fun hash ->
  let id = { Commit_id.repo; gref; hash } in
  Lwt.return @@ Ok { Commit.repo = local_repo; id }

let pp f key = Fmt.pf f "git clone %a" Key.pp key

let auto_cancel = false
