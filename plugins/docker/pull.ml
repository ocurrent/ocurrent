open Lwt.Infix

type t = No_context

let ( >>!= ) = Lwt_result.bind

module Key = struct
  type t = {
    docker_context : string option;
    arch: string option;
    tag : string;
  } [@@deriving to_yojson]

  let cmd { docker_context; tag; _ } = Cmd.docker ~docker_context ["pull"; tag]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = Image

let id = "docker-pull"

let get_digest_from_manifest manifest arch =
  let open Yojson.Basic.Util in
  try Yojson.Basic.from_string manifest |>
      member "manifests" |> to_list |>
      List.find (fun j -> member "platform" j |> fun j ->
                          (member "architecture" j |> to_string = arch) &&
                          (member "os" j |> to_string = "linux")) |>
      member "digest" |> fun digest -> Ok (to_string digest)
  with _ -> Error (`Msg (Fmt.strf "failed to parse docker manifest to find arch: %S" manifest))

let build No_context job key =
  Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
  let { Key.docker_context; tag; arch } = key in
  match arch with
  | None -> begin
      Current.Process.exec ~cancellable:true ~job (Key.cmd key) >>!= fun () ->
      let cmd = Cmd.docker ~docker_context ["image"; "inspect"; tag; "-f"; "{{index .RepoDigests 0}}"] in
      Current.Process.check_output ~cancellable:false ~job cmd >>!= fun id ->
      let id = String.trim id in
      Current.Job.log job "Pulled %S -> %S" tag id;
      Lwt_result.return (Image.of_hash id)
    end
  | Some arch -> begin
      let cmd = Cmd.docker ~docker_context ["manifest"; "inspect"; tag ] in
      Current.Process.check_output ~cancellable:true ~job cmd >>!= fun manifest ->
      match get_digest_from_manifest manifest arch with
      | Error _ as e -> Lwt.return e
      | Ok hash ->
        let full_tag = tag ^ "@" ^ hash in
        Current.Process.exec ~cancellable:true ~job (Key.cmd {key with Key.tag=full_tag}) >>!= fun () ->
        Lwt_result.return (Image.of_hash full_tag)
    end

let pp f key = Cmd.pp f (Key.cmd key)

let auto_cancel = false
