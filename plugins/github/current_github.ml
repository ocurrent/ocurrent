open Lwt.Infix

module Repo_id = Repo_id
module Api = Api
module App = App
module Installation = Installation
module Auth = Auth

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let webhook_events_total =
    let help = "Incoming webhook events" in
    Counter.v_label ~label_name:"event" ~help ~namespace ~subsystem "webhook_events_total"
end

let validate_webhook_payload webhook_secret body headers event =
  let request_signature = Option.value ~default:"<empty>" (Cohttp.Header.get headers "X-Hub-Signature-256") in
  let signature = "sha256=" ^ Hex.show @@ Hex.of_cstruct @@
    Mirage_crypto.Hash.SHA256.(hmac ~key:(Cstruct.of_string webhook_secret) (Cstruct.of_string body)) in
  if Eqaf.equal signature request_signature then
    Ok ()
  else
    let s = Printf.sprintf {|
Invalid X-Hub-Signature-256 received for %s, expecting %s == %s.
Please check the Webhook secrets are setup appropriately.
See https://docs.github.com/en/developers/webhooks-and-events/webhooks/securing-your-webhooks |}
        event signature request_signature in
    Error s

let webhook ~engine ~get_job_ids ~webhook_secret ~has_role = object
  inherit Current_web.Resource.t

  method! post_raw _site req body =
    Log.info (fun f -> f "input_webhook: %a" Cohttp_lwt.Request.pp_hum req);
    let headers = Cohttp.Request.headers req in
    let event = Cohttp.Header.get headers "X-GitHub-Event" in
    let event_str  = Option.value ~default:"NONE" event in
    Log.info (fun f -> f "Got GitHub event %S" event_str);
    Prometheus.Counter.inc_one (Metrics.webhook_events_total event_str);
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let json_body = Yojson.Safe.from_string body in
    match validate_webhook_payload webhook_secret body headers event_str with
    | Error msg ->
      Log.warn (fun f -> f "%s" msg);
      Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized ~body:"Invalid X-Hub-Signature-256" ()
    | Ok () ->
      begin match event with
        | Some "installation_repositories" -> Installation.input_installation_repositories_webhook ()
        | Some "installation" -> App.input_installation_webhook ()
        | Some ("pull_request" | "push" | "create") -> Api.input_webhook json_body
        | Some "check_run" -> Api.rebuild_webhook ~engine ~get_job_ids ~has_role json_body
        | Some x -> Log.warn (fun f -> f "Unknown GitHub event type %S" x)
        | None -> Log.warn (fun f -> f "Missing GitHub event type in webhook!")
      end;
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()
end
