open Lwt.Infix

type t = {
  pull : bool;
  pool : unit Lwt_pool.t option;
  timeout : Duration.t option;
}

let id = "docker-build"

let use_pool pool f =
  match pool with
  | None -> f ()
  | Some pool ->
    Lwt_pool.use pool f

module Key = struct
  type t = {
    commit : [ `No_context | `Git of Current_git.Commit.t ];
    dockerfile : string option;
    docker_context : string option;
    squash : bool;
  }

  let digest_dockerfile = function
    | None -> None
    | Some contents -> Some (Digest.string contents |> Digest.to_hex)
                         
  let source_to_json = function
    | `No_context -> `Null
    | `Git commit -> `String (Current_git.Commit.id commit)

  let to_json { commit; dockerfile; docker_context; squash } =
    `Assoc [
      "commit", source_to_json commit;
      "dockerfile", [%derive.to_yojson:string option] (digest_dockerfile dockerfile);
      "docker_context", [%derive.to_yojson:string option] docker_context;
      "squash", [%derive.to_yojson:bool] squash;
    ]

  let digest t = Yojson.Safe.to_string (to_json t)

  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = Image

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let or_raise = function
  | Ok () -> ()
  | Error (`Msg m) -> raise (Failure m)

let with_context ~switch ~job context fn =
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
  | `Git commit -> Current_git.with_checkout ~switch ~job commit fn

let build ~switch { pull; pool; timeout } job key =
  use_pool pool @@ fun () ->
  Current.Job.set_running ?timeout job;
  let { Key.commit; docker_context; dockerfile; squash } = key in
  with_context ~switch ~job commit @@ fun dir ->
  begin match dockerfile with
    | None -> ()
    | Some contents ->
      Current.Job.log job "@[<v2>Using Dockerfile:@,%a@]" Fmt.lines contents;
      Bos.OS.File.write Fpath.(dir / "Dockerfile") (contents ^ "\n") |> or_raise;
  end;
  let pull = if pull then ["--pull"] else [] in
  let squash = if squash then ["--squash"] else [] in
  let iidfile = Fpath.add_seg dir "docker-iid" in
  let cmd = Cmd.docker ~docker_context @@ ["build"] @
                                          pull @ squash @
                                          ["--iidfile";
                                           Fpath.to_string iidfile; "--";
                                           Fpath.to_string dir] in
  Current.Process.exec ~switch ?stdin:dockerfile ~job cmd >|= function
  | Error _ as e -> e
  | Ok () ->
    Bos.OS.File.read iidfile |> Stdlib.Result.map @@ fun hash ->
    Log.info (fun f -> f "Built docker image %s" hash);
    Image.of_hash hash

let pp f key = Fmt.pf f "@[<v2>docker build %a@]" Key.pp key

let auto_cancel = true

let level _ _ = Current.Level.Average
