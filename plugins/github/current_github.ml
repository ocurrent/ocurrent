module Repo_id = Repo_id
module Api = Api
module App = App
module Installation = Installation

let input_webhook req _body =
  Log.info (fun f -> f "input_webhook: %a" Cohttp_lwt.Request.pp_hum req);
  let headers = Cohttp.Request.headers req in
  let event = Cohttp.Header.get headers "X-GitHub-Event" in
  Log.info (fun f -> f "Got GitHub event %a" Fmt.(option ~none:(unit "NONE") (quote string)) event);
  begin match event with
    | Some "installation_repositories" -> Lwt_condition.broadcast Installation.installation_repositories_cond ()
    | Some "installation" -> Lwt_condition.broadcast App.installations_changed_cond ()
    | _ -> Lwt_condition.broadcast Api.webhook_cond ()
  end;
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()
