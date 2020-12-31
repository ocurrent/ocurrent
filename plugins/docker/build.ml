open Lwt.Infix

type t = {
  pull : bool;
  pool : unit Current.Pool.t option;
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
    dockerfile : [`File of Fpath.t | `Contents of string];
    docker_context : string option;
    squash : bool;
    build_args: string list;
  }

  let digest_dockerfile = function
    | `File name -> `Assoc [ "file", `String (Fpath.to_string name) ]
    | `Contents contents -> `Assoc [ "contents", `String (Digest.string contents |> Digest.to_hex) ]

  let source_to_json = function
    | `No_context -> `Null
    | `Git commit -> `String (Current_git.Commit.hash commit)

  let to_json { commit; dockerfile; docker_context; squash; build_args } =
    `Assoc [
      "commit", source_to_json commit;
      "dockerfile", digest_dockerfile dockerfile;
      "docker_context", [%derive.to_yojson:string option] docker_context;
      "squash", [%derive.to_yojson:bool] squash;
      "build_args", [%derive.to_yojson:string list] build_args;
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

let with_context ~job context fn =
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
  | `Git commit -> Current_git.with_checkout ~job commit fn

let build { pull; pool; timeout } job key =
  let { Key.commit; docker_context; dockerfile; squash; build_args } = key in
  begin match dockerfile with
    | `Contents contents ->
      Current.Job.log job "@[<v2>Using Dockerfile:@,%a@]" Fmt.lines contents
    | `File _ -> ()
  end;
  Current.Job.start ?timeout ?pool job ~level:Current.Level.Average >>= fun () ->
  with_context ~job commit @@ fun dir ->
  let file =
    match dockerfile with
    | `Contents contents ->
      Bos.OS.File.write Fpath.(dir / "Dockerfile") (contents ^ "\n") |> or_raise;
      []
    | `File name ->
      ["-f"; Fpath.(to_string (dir // name))]
  in
  let pull = if pull then ["--pull"] else [] in
  let squash = if squash then ["--squash"] else [] in
  let iidfile = Fpath.add_seg dir "docker-iid" in
  let cmd = Cmd.docker ~docker_context @@ ["build"] @
                                          pull @ squash @ build_args @ file @
                                          ["--iidfile";
                                           Fpath.to_string iidfile; "--";
                                           Fpath.to_string dir] in
  let pp_error_command f = Fmt.string f "Docker build" in
  Current.Process.exec ~cancellable:true ~pp_error_command ~job cmd >|= function
  | Error _ as e -> e
  | Ok () ->
    Bos.OS.File.read iidfile |> Stdlib.Result.map @@ fun hash ->
    Log.info (fun f -> f "Built docker image %s" hash);
    Image.of_hash hash

let pp f key = Fmt.pf f "@[<v2>docker build %a@]" Key.pp key

let auto_cancel = true
