open Lwt.Infix

type t = {
  pull : bool;
}

let id = "docker-build"

module Key = struct
  type t = {
    commit : Current_git.Commit.t;
    dockerfile : string option;
  } [@@deriving ord]

  let digest { commit; dockerfile } =
    let dockerfile =
      match dockerfile with
      | None -> ""
      | Some contents -> "-" ^ (Digest.string contents |> Digest.to_hex)
    in
    Current_git.Commit.id commit ^ dockerfile

  let pp f t = Fmt.string f (digest t)
end

module Value = Image

let docker_tag t = Fmt.strf "build-of-%s" (Key.digest t)

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let build ~switch { pull } job key =
  let { Key.commit; dockerfile } = key in
  let tag = docker_tag key in
  Current_git.with_checkout ~job commit @@ fun dir ->
  let f =
    match dockerfile with
    | None -> "Dockerfile"
    | Some _ -> "-"
  in
  let opts = if pull then ["--pull"] else [] in
  let cmd = ["docker"; "build"] @ opts @ ["-f"; f; "-t"; tag; "--"; Fpath.to_string dir] in
  Current_cache.Process.exec ~switch ?stdin:dockerfile ~job ("", Array.of_list cmd) >|= function
  | Ok () ->
    Log.info (fun f -> f "Build of docker image %S succeeded" tag);
    Ok tag
  | Error _ as e -> e

let pp f key = Fmt.pf f "docker build %a" Key.pp key

let auto_cancel = true

let level _ _ = Current.Level.Average
