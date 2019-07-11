open Lwt.Infix

type t = {
  pull : bool;
}

let id = "docker-build"

module Key = struct
  type t = {
    commit : Current_git.Commit.t;
    dockerfile : string option;
    docker_host : string option;
  }

  let digest_dockerfile = function
    | None -> None
    | Some contents -> Some (Digest.string contents |> Digest.to_hex)

  let to_json { commit; dockerfile; docker_host } =
    `Assoc [
      "commit", `String (Current_git.Commit.id commit);
      "dockerfile", [%derive.to_yojson:string option] (digest_dockerfile dockerfile);
      "docker_host", [%derive.to_yojson:string option] docker_host;
    ]

  let digest t = Yojson.Safe.to_string (to_json t)

  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = Image

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let build ~switch { pull } job key =
  let { Key.commit; docker_host; dockerfile } = key in
  Current_git.with_checkout ~switch ~job commit @@ fun dir ->
  let f =
    match dockerfile with
    | None -> []
    | Some contents ->
      Current.Job.log job "@[<v2>Using Dockerfile:@,%a@]" Fmt.lines contents;
      ["-f"; "-"]
  in
  let pull = if pull then ["--pull"] else [] in
  let iidfile = Fpath.add_seg dir "docker-iid" in
  let cmd = Cmd.docker ~docker_host @@ ["build"] @
                                       pull @ f @
                                       ["--iidfile";
                                        Fpath.to_string iidfile; "--";
                                        Fpath.to_string dir] in
  Current.Process.exec ~switch ?stdin:dockerfile ~job cmd >|= function
  | Error _ as e -> e
  | Ok () ->
    match Bos.OS.File.read iidfile with
    | Ok hash ->
      Log.info (fun f -> f "Built docker image %s" hash);
      Ok (Image.of_hash hash)
    | Error _ as e -> e

let pp f key = Fmt.pf f "@[<v2>docker build %a@]" Key.pp key

let auto_cancel = true

let level _ _ = Current.Level.Average
