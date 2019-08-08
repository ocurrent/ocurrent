open Lwt.Infix

type auth = Push.auth

type t = auth option

let ( >>!= ) = Lwt_result.bind

let id = "docker-push-manifest"

(* Pushing can fail with "malformed MIME header line: Too Many Requests (HAP429)",
   so limit to one at a time. *)
let push_mutex = Lwt_mutex.create ()

module Key = Current.String

module Value = struct
  type t = {
    manifests : S.repo_id list;
  }

  let digest { manifests } =
    Yojson.Safe.to_string @@ `Assoc [
      "manifests", `List (List.map (fun id -> `String id) manifests);
    ]
end

module Outcome = Current.Unit

let create_cmd ~tag {Value.manifests} =
  let args = ["docker"; "manifest"; "create"; tag] @ manifests in
  ("", Array.of_list args)

let push_cmd tag =
  let args = ["docker"; "manifest"; "push"; tag] in
  ("", Array.of_list args)

let publish ~switch auth job tag value =
  Current.Process.exec ~switch ~job (create_cmd ~tag value) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    Lwt_mutex.with_lock push_mutex @@ fun () ->
    begin match auth with
      | None -> Lwt.return (Ok ())
      | Some (user, password) ->
        let cmd = Cmd.login ~docker_context:None ~user in
        Current.Process.exec ~switch ~job ~stdin:password cmd
    end >>!= fun () ->
    Current.Process.exec ~switch ~job (push_cmd tag)

let pp f (tag, value) =
  Fmt.pf f "push %s = %s" tag (Value.digest value)

let auto_cancel = true

let level _auth _tag _value = Current.Level.Dangerous
