type t = No_context

let ( >>!= ) = Lwt_result.bind

module Key = struct
  type t = {
    repo : string;  (* Remote repository from which to pull. *)
    gref : string;
  } [@@deriving to_yojson]

  let pp f t = Yojson.Safe.pretty_print f (to_yojson t)

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Commit

let id = "git-clone"

let build ~switch No_context job { Key.repo; gref } =
  let local_repo = Cmd.local_copy repo in
  (* Ensure we have a local clone of the repository. *)
  begin
    if Cmd.dir_exists local_repo
    then Cmd.git_fetch ~switch ~job ~src:repo ~dst:local_repo (Fmt.strf "%s:refs/remotes/origin/%s" gref gref)
    else Cmd.git_clone ~switch ~job ~src:repo local_repo
  end >>!= fun () ->
  Cmd.git_rev_parse ~switch ~job ~repo:local_repo ("origin/" ^ gref) >>!= fun hash ->
  let id = { Commit_id.repo; gref; hash } in
  Lwt.return @@ Ok { Commit.repo = local_repo; id }

let pp f key = Fmt.pf f "git clone %a" Key.pp key

let auto_cancel = false

let level _ _ = Current.Level.Mostly_harmless
