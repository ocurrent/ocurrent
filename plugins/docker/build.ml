type build = {
  pull : bool;
  pool : unit Current.Pool.t option;
  timeout : Duration.t option;
  level : Current.Level.t option;
}

type t = build * Eio.Fs.dir Eio.Path.t * Eio.Process.mgr

let (>>?=) = Result.bind

let id = "docker-build"

let use_pool pool f =
  match pool with
  | None -> f ()
  | Some pool ->
    Lwt_pool.use pool f

module Key = struct
  type t = {
    commit : [ `No_context | `Git of Current_git.Commit.t | `Dir of Fpath.t ];
    dockerfile : [`File of Fpath.t | `Contents of string];
    docker_context : string option;
    squash : bool;
    buildx: bool;
    build_args: string list;
    path : Fpath.t option;
  }

  let digest_dockerfile = function
    | `File name -> `Assoc [ "file", `String (Fpath.to_string name) ]
    | `Contents contents -> `Assoc [ "contents", `String (Digest.string contents |> Digest.to_hex) ]

  let source_to_json = function
    | `No_context -> `Null
    | `Git commit -> `String (Current_git.Commit.hash commit)
    | `Dir path -> `String (Fpath.to_string path)

  let to_json { commit; dockerfile; docker_context; squash; buildx; build_args; path } =
    `Assoc [
      "commit", source_to_json commit;
      "dockerfile", digest_dockerfile dockerfile;
      "docker_context", [%derive.to_yojson:string option] docker_context;
      "squash", [%derive.to_yojson:bool] squash;
      "buildx", [%derive.to_yojson:bool] buildx;
      "build_args", [%derive.to_yojson:string list] build_args;
      "path", Option.(value ~default:`Null (map (fun v -> `String (Fpath.to_string v)) path));
    ]

  let digest t = Yojson.Safe.to_string (to_json t)

  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = Image

let errorf fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let or_raise = function
  | Ok () -> ()
  | Error (`Msg m) -> raise (Failure m)

let with_context ~fs ~job context proc fn =
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fs fn
  | `Dir path ->
      Current.Process.with_tmpdir ~prefix:"build-context-" fs @@ fun cwd ->
      Current.Process.exec ~cwd ~cancellable:true ~job proc ("", [ "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." ]) >>?= fun () ->
      fn cwd
  | `Git commit -> Current_git.with_checkout ~fs ~job proc commit fn

let build ({ pull; pool; timeout; level }, fs, proc) job key =
  let { Key.commit; docker_context; dockerfile; squash; buildx; build_args; path } = key in
  begin match dockerfile with
    | `Contents contents ->
      Current.Job.log job "@[<v2>Using Dockerfile:@,%a@]" Fmt.lines contents
    | `File _ -> ()
  end;
  let level = Option.value level ~default:Current.Level.Average in
  Current.Job.start ?timeout ?pool job ~level;
  with_context ~job ~fs commit proc @@ fun dir ->
  let dir = match path with
    | Some path -> Eio.Path.(dir / Fpath.to_string path)
    | None -> dir
  in
  let file =
    match dockerfile with
    | `Contents contents ->
      Eio.Path.(save ~create:(`If_missing 0o644) (dir / "Dockerfile") (contents ^ "\n"));
      []
    | `File name ->
      ["-f"; (snd Eio.Path.(dir / Fpath.to_string name))]
  in
  let pull = if pull then ["--pull"] else [] in
  let squash = if squash then ["--squash"] else [] in
  let buildx = if buildx then ["buildx"] else [] in
  let iidfile = Eio.Path.(dir / "docker-iid") in
  let cmd = Cmd.docker ~docker_context (
    buildx @ ["build"] @
    pull @ squash @ build_args @ file @
    ["--iidfile";
     snd iidfile; "--";
     snd dir])
  in
  let pp_error_command f = Fmt.string f "Docker build" in
  match Current.Process.exec ~cancellable:true ~pp_error_command ~job proc cmd with
  | Error _ as e -> e
  | Ok () ->
    let hash = Eio.Path.load iidfile in
    Log.info (fun f -> f "Built docker image %s" hash);
    Ok (Image.of_hash hash)

let pp f key = Fmt.pf f "@[<v2>docker build %a@]" Key.pp key

let auto_cancel = true
