let src = Logs.Src.create "current_web" ~doc:"OCurrent web interface"
module Log = (val Logs.src_log src : Logs.LOG)

type webhook = string * (Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)

let metrics ~engine = object
  inherit Resource.t

  method! private get _request =
    Current.Engine.(update_metrics engine);
    let data = Prometheus.CollectorRegistry.(collect default) in
    let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
    let headers = Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4" in
    Utils.Server.respond_string ~status:`OK ~headers ~body ()
end

let set_confirm ~engine = object
  inherit Resource.t

  method! private post _request body =
    let data = Uri.query_of_encoded body in
    let config = Current.Engine.config engine in
    match List.assoc_opt "level" data |> Option.value ~default:[] with
    | ["none"] ->
      Current.Config.set_confirm config None;
      Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
    | [level] ->
      begin match Current.Level.of_string level with
        | Error (`Msg msg) -> Utils.respond_error `Bad_request msg
        | Ok level ->
          Current.Config.set_confirm config (Some level);
          Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
      end
    | _ -> Utils.respond_error `Bad_request "Missing level"
end

let webhooks ~webhooks hook = object
  inherit Resource.t

  method! post_raw request body =
    begin match List.assoc_opt hook (webhooks : webhook list) with
      | Some f -> f request body
      | None -> Utils.Server.respond_not_found ()
    end
end

let routes ~engine ~webhooks:h =
  let open Routes in
  Routes.one_of @@ [
    nil @--> Main.r ~engine;
    s "index.html" /? nil @--> Main.r ~engine;
    s "css" / s "style.css" /? nil @--> Style.r;
    s "pipeline.svg" /? nil @--> Pipeline.r ~engine;
    s "query" /? nil @--> Query.r;
    s "log-rules" /? nil @--> Log_rules.r;
    s "metrics" /? nil @--> metrics ~engine;
    s "set" / s "confirm" /? nil @--> set_confirm ~engine;
    s "webhooks" / str /? nil @--> webhooks ~webhooks:h;
    s "jobs" /? nil @--> Jobs.r;
  ] @ Job.routes ~engine

let handle_request ~engine ~webhooks _conn request body =
  match Lwt.state (Current.Engine.thread engine) with
  | Lwt.Fail ex ->
    let body = Fmt.strf "Engine has crashed: %a" Fmt.exn ex in
    Utils.respond_error `Internal_server_error body
  | Lwt.Return `Cant_happen -> assert false
  | Lwt.Sleep ->
    let meth = Cohttp.Request.meth request in
    let uri = Cohttp.Request.uri request in
    let path = Uri.path uri in
    Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
    match Routes.match' (routes ~engine ~webhooks) ~target:path with
    | None -> Utils.Server.respond_not_found ()
    | Some resource ->
      match meth with
      | `GET -> resource#get_raw request
      | `POST -> resource#post_raw request body
      | (`HEAD | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `PATCH | `Other _) ->
        Utils.respond_error `Bad_request "Bad method"

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let default_mode = `TCP (`Port 8080)

let show_webhook (name, _) =
  Logs.info (fun f -> f "Registered webhook at /webhooks/%s" name)

let run ?(mode=default_mode) ?(webhooks=[]) engine =
  let config = Utils.Server.make ~callback:(handle_request ~engine ~webhooks) () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  List.iter show_webhook webhooks;
  Lwt.try_bind
    (fun () -> Utils.Server.create ~mode config)
    (fun () -> Lwt.return @@ Error (`Msg "Web-server stopped!"))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
        let msg = Fmt.strf "Web-server failed.@ Another program is already using this port %a." pp_mode mode in
        Lwt.return @@ Error (`Msg msg)
      | ex -> Lwt.fail ex
    )

open Cmdliner

let port =
  Arg.value @@
  Arg.opt Arg.int 8080 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]

let make port = `TCP (`Port port)

let cmdliner =
  Term.(const make $ port)
