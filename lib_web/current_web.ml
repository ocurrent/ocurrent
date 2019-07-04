open Lwt.Infix

let src = Logs.Src.create "current_web" ~doc:"OCurrent web interface"
module Log = (val Logs.src_log src : Logs.LOG)

module Server = Cohttp_lwt_unix.Server

let dot_to_svg = ("", [| "dot"; "-Tsvg" |])

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let render_svg a =
  let dotfile = Fmt.to_to_string Current.Analysis.pp_dot a in
  let proc = Lwt_process.open_process_full dot_to_svg in
  Lwt_io.write proc#stdin dotfile >>= fun () ->
  Lwt_io.close proc#stdin >>= fun () ->
  Lwt_io.read proc#stdout >>= fun svg ->
  proc#status >|= function
  | Unix.WEXITED 0 -> Ok svg
  | Unix.WEXITED i -> errorf "dot failed (exit status %d) - is graphviz installed?" i
  | Unix.WSTOPPED i
  | Unix.WSIGNALED i -> errorf "dot crashed (signal %d)" i

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let handle_request ~engine _conn request _body =
  match Lwt.state (Current.Engine.thread engine) with
  | Lwt.Fail ex ->
    let body = Fmt.strf "Engine has crashed: %a" Fmt.exn ex in
    Server.respond_error ~status:`Internal_server_error ~body ()
  | Lwt.Return `Cant_happen -> assert false
  | Lwt.Sleep ->
    let meth = Cohttp.Request.meth request in
    let path = Cohttp.Request.resource request in
    Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
    match meth, path with
    | `GET, "/" | `GET, "/index.html" ->
      let state = Current.Engine.state engine in
      let body = html_to_string (Main.render state) in
      Server.respond_string ~status:`OK ~body ()
    | `GET, "/pipeline.svg" ->
      begin
        let state = Current.Engine.state engine in
        render_svg state.Current.Engine.analysis >>= function
        | Ok body ->
          let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
          Server.respond_string ~status:`OK ~headers ~body ()
        | Error (`Msg msg) ->
          let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
          Server.respond_error ~status:`Internal_server_error ~headers ~body:msg ()
      end
    | _ ->
      Server.respond_not_found ()

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let default_mode = `TCP (`Port 8080)

let run ?(mode=default_mode) engine =
  let config = Server.make ~callback:(handle_request ~engine) () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  Server.create ~mode config

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
