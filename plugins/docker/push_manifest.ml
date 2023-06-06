type auth = Push.auth

type t = (auth option * Eio.Fs.dir Eio.Path.t * Eio.Process.mgr)

let ( >>!= ) = Result.bind

let id = "docker-push-manifest"

(* Pushing can fail with "malformed MIME header line: Too Many Requests (HAP429)",
   so limit to one at a time. *)
let push_mutex = Eio.Mutex.create ()

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

let push_cmd ~config tag =
  Cmd.docker ~config ~docker_context:None ["manifest"; "push"; tag]

let or_fail = function
  | Ok x -> x
  | Error (`Msg x) -> failwith x

let publish (auth, fs, proc) job tag value =
  Current.Job.start job ~level:Current.Level.Dangerous;
  Current.Process.with_tmpdir ~prefix:"push-manifest" fs @@ fun config ->
  Eio.Path.(save ~create:(`If_missing 0o644) (config / "config.json") {|{"experimental": "enabled"}|});
  begin match auth with
    | None -> Ok ()
    | Some (user, password) ->
      let cmd = Cmd.login ~config ~docker_context:None user in
      Current.Process.exec ~cancellable:true ~job ~stdin:password proc cmd
  end >>!= fun () ->
  match Current.Process.exec ~cancellable:true ~job proc (create_cmd ~config ~tag value) with
  | Error _ as e -> e
  | Ok () ->
    Eio.Mutex.use_ro push_mutex @@ fun () ->
    Current.Process.check_output ~cancellable:true ~job proc (push_cmd ~config tag) >>!= fun output ->
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
    Ok repo_id

let pp f (tag, value) =
  Fmt.pf f "push %s = %s" tag (Value.digest value)

let auto_cancel = true
