open Lwt.Infix

let dot_to_svg = ("", [| "dot"; "-Tsvg" |])

let render_svg ctx a =
  let uri = Context.uri ctx in
  let env = Uri.query uri |> List.filter_map (function
      | (_, []) -> None
      | (k, v :: _) -> Some (k, v)
    ) in
  let old_query = Uri.query uri in
  let url = function
    | `Collapse (k, v) ->
      let query = (k, [v]) :: List.remove_assoc k old_query in
      Some (Uri.make ~path:"/" ~query () |> Uri.to_string)
    | `Job id -> Some (Fmt.strf "/job/%s" id)
  in
  let dotfile = Fmt.to_to_string (Current.Analysis.pp_dot ~env ~url) a in
  let proc = Lwt_process.open_process_full dot_to_svg in
  Lwt_io.write proc#stdin dotfile >>= fun () ->
  Lwt_io.close proc#stdin >>= fun () ->
  Lwt_io.read proc#stdout >>= fun svg ->
  proc#status >|= function
  | Unix.WEXITED 0 -> Ok svg
  | Unix.WEXITED i -> Fmt.error_msg "dot failed (exit status %d) - is graphviz installed?" i
  | Unix.WSTOPPED i
  | Unix.WSIGNALED i -> Fmt.error_msg "dot crashed (signal %d)" i

let r ~engine = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    render_svg ctx (Current.Engine.pipeline engine) >>= function
    | Ok body ->
      let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
      Utils.Server.respond_string ~status:`OK ~headers ~body ()
    | Error (`Msg msg) ->
      Context.respond_error ctx `Internal_server_error msg
end
