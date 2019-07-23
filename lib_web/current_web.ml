open Lwt.Infix
open Astring

let src = Logs.Src.create "current_web" ~doc:"OCurrent web interface"
module Log = (val Logs.src_log src : Logs.LOG)

module Server = Cohttp_lwt_unix.Server

let dot_to_svg = ("", [| "dot"; "-Tsvg" |])

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let respond_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  Server.respond_error ~status ~headers ~body ()

let get_job date log =
  let job_id = Fmt.strf "%s/%s" date log in
  match Current.Job.log_path job_id with
  | Error (`Msg msg) -> respond_error `Bad_request msg
  | Ok path ->
    match Bos.OS.File.read path with
    | Error (`Msg msg) -> respond_error `Internal_server_error msg
    | Ok body ->
      let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
      Server.respond_string ~status:`OK ~headers ~body ()

let render_svg a =
  let url id = Some (Fmt.strf "/job/%s" id) in
  let dotfile = Fmt.to_to_string (Current.Analysis.pp_dot ~url) a in
  let proc = Lwt_process.open_process_full dot_to_svg in
  Lwt_io.write proc#stdin dotfile >>= fun () ->
  Lwt_io.close proc#stdin >>= fun () ->
  Lwt_io.read proc#stdout >>= fun svg ->
  proc#status >|= function
  | Unix.WEXITED 0 -> Ok svg
  | Unix.WEXITED i -> errorf "dot failed (exit status %d) - is graphviz installed?" i
  | Unix.WSTOPPED i
  | Unix.WSIGNALED i -> errorf "dot crashed (signal %d)" i

let handle_request ~engine _conn request _body =
  match Lwt.state (Current.Engine.thread engine) with
  | Lwt.Fail ex ->
    let body = Fmt.strf "Engine has crashed: %a" Fmt.exn ex in
    respond_error `Internal_server_error body
  | Lwt.Return `Cant_happen -> assert false
  | Lwt.Sleep ->
    let meth = Cohttp.Request.meth request in
    let path = Cohttp.Request.resource request in
    Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
    match meth, String.cuts ~sep:"/" ~empty:false path with
    | `GET, ([] | ["index.html"]) ->
      let state = Current.Engine.state engine in
      let body = Main.dashboard state in
      Server.respond_string ~status:`OK ~body ()
    | `GET, ["job"; date; log] ->
      get_job date log
    | `GET, ["css"; "style.css"] ->
      Style.get ()
    | `GET, ["pipeline.svg"] ->
      begin
        let state = Current.Engine.state engine in
        render_svg state.Current.Engine.analysis >>= function
        | Ok body ->
          let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
          Server.respond_string ~status:`OK ~headers ~body ()
        | Error (`Msg msg) ->
          respond_error `Internal_server_error msg
      end
    | _ ->
      Server.respond_not_found ()

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let default_mode = `TCP (`Port 8080)

let run ?(mode=default_mode) engine =
  let config = Server.make ~callback:(handle_request ~engine) () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  Lwt.try_bind
    (fun () -> Server.create ~mode config)
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
