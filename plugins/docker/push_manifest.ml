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

module Outcome = struct
  include Current.String

  let unmarshal = function
    | "()" -> failwith "Result from old version. Need rebuild"
    | repo_id -> repo_id
end

let create_cmd ~config ~tag {Value.manifests} =
  Cmd.docker ~config ~docker_context:None (["manifest"; "create"; tag] @ manifests)

let docker_manifest ~job ~config tag =
  let retry_attempts = 5 in
  let cmd = Cmd.docker ~config ~docker_context:None ["manifest"; "push"; tag] in
  let operation () =
    Current.Process.check_output ~cancellable:true ~job cmd
    >|= Result.map_error (fun (`Msg err) -> `Retry err)
  in
  Current.Retry.(operation |> on_error |> with_sleep |> n_times retry_attempts)

let or_fail = function
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let publish auth job tag value =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  Current.Process.with_tmpdir ~prefix:"push-manifest" @@ fun config ->
  Bos.OS.File.write Fpath.(config / "config.json") {|{"experimental": "enabled"}|} |> or_fail;
  begin match auth with
    | None -> Lwt.return (Ok ())
    | Some (user, password) ->
      let cmd = Cmd.login ~config ~docker_context:None user in
      Current.Process.exec ~cancellable:true ~job ~stdin:password cmd
  end >>!= fun () ->
  Prometheus.Gauge.inc_one Metrics.docker_push_manifest_events;
  Current.Process.exec ~cancellable:true ~job (create_cmd ~config ~tag value) >>= (function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    Lwt_mutex.with_lock push_mutex @@ fun () ->
    docker_manifest ~job ~config tag >>!= fun output ->
    (* docker-manifest is still experimental and doesn't have a sensible output format yet. *)
    Current.Job.write job output;
    let output = String.trim output in
    let hash =
      match Astring.String.cut ~rev:true ~sep:"\n" output with
      | None -> output
      | Some (_, id) -> id
    in
    let repo_id = Printf.sprintf "%s@%s" tag hash in
    Current.Job.log job "--> %S" repo_id;
    Lwt_result.return repo_id)
  >|= (fun res -> Prometheus.Gauge.dec_one Metrics.docker_push_manifest_events; res)

let pp f (tag, value) =
  Fmt.pf f "push %s = %s" tag (Value.digest value)

let auto_cancel = true
