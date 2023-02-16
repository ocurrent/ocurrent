open Lwt.Infix

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let forbidden (ctx : Context.t) =
  match ctx.site.authn, ctx.user with
  | None, _          (* Site doesn't allow logins! *)
  | _, Some _ ->     (* User is already logged in. *)
    Context.respond_error ctx `Forbidden "Permission denied"
  | Some login_uri, None ->
    let uri = login_uri ~csrf:(Context.csrf ctx) in
    Context.respond_ok ctx Tyxml.Html.[
        txt "Permission denied - you need to ";
        a ~a:[a_href (Uri.to_string uri)] [ txt "log in" ]
    ]

class virtual t = object (self : #Site.raw_resource)
  val can_get : Role.t = `Admin
  val can_post : Role.t = `Admin

  method private get ctx =
    Context.respond_error ctx `Bad_request "Bad method"

  method private post ctx (_ : string) =
    Context.respond_error ctx `Bad_request "Bad method"

  method private post_multipart ctx (_ : string Multipart_form.elt list) =
    Context.respond_error ctx `Bad_request "Bad method"

  method get_raw site request =
    Context.of_request ~site request >>= fun ctx ->
    if Context.has_role ctx can_get then self#get ctx
    else forbidden ctx

  method post_raw site request body =
    Context.of_request ~site request >>= fun ctx ->
    if Context.has_role ctx can_post then (
      match Cohttp.(Header.get (Request.headers request)) "Content-Type" with
      | None -> Context.respond_error ctx `Bad_request "Unset Content-Type on POST request"
      | Some content_type ->
        match Multipart_form.Content_type.of_string (content_type ^ "\r\n") with
        | Error (`Msg e) -> Context.respond_error ctx `Bad_request e
        | Ok ({ty = `Multipart; subty = `Iana_token "form-data"; _} as content_type) ->
          let body = Cohttp_lwt.Body.to_stream body in
          Multipart_form_lwt.of_stream_to_tree body content_type >>= fun tree ->
          begin match tree with
            | Error (`Msg e) -> Context.respond_error ctx `Bad_request e
            | Ok multipart ->
              let multipart = Multipart_form.flatten multipart in
              let csrf, others = List.partition (fun {Multipart_form.header; _} ->
                                     match Multipart_form.Header.content_disposition header with
                                     | Some header -> Multipart_form.Content_disposition.name header = Some "csrf"
                                     | _ -> false) multipart in
              begin match csrf with
                | [got] when got.Multipart_form.body = Context.csrf ctx ->
                  self#post_multipart ctx others
                | _ -> Context.respond_error ctx `Bad_request "Bad CSRF token"
              end
          end
        | Ok _ ->
          Cohttp_lwt.Body.to_string body >>= fun body ->
          let data = Uri.query_of_encoded body in
          match List.assoc_opt "csrf" data |> Option.value ~default:[] with
          | [got] when got = Context.csrf ctx ->
            self#post ctx body
          | _ -> Context.respond_error ctx `Bad_request "Bad CSRF token"
    ) else (
      forbidden ctx
    )

  method nav_link = None
end

let render_logged_out ctx =
  Context.respond_ok ctx Tyxml.Html.[ txt "You are now logged out" ]

let logout = object
  method get_raw site request =
    Context.of_request ~site request >>= fun ctx ->
    Context.respond_error ctx `Bad_request "Use a POST to log out"

  method post_raw site request body =
    Context.of_request ~site request >>= fun ctx ->
    if ctx.user = None then render_logged_out ctx
    else (
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let data = Uri.query_of_encoded body in
      match List.assoc_opt "csrf" data |> Option.value ~default:[] with
      | [got] when got = Context.csrf ctx ->
        Site.Sess.clear site.session_backend ctx.session >>= fun () ->
        render_logged_out { ctx with user = None }
      | _ -> Context.respond_error ctx `Bad_request "Bad CSRF token"
    )

  method nav_link = None
end

(* Serve a static asset from an OCaml string, with a cache of one day
   (by default). *)
let static ~content_type ?(max_age=86400) body = object
  inherit t

  val! can_get = `Viewer

  method! private get _ctx =
    let headers =
      Cohttp.Header.of_list [
          ("Content-Type", content_type);
          ("Cache-Control", Printf.sprintf "public, max-age=%d;" max_age);
        ]
    in
    Utils.Server.respond_string ~status:`OK ~headers ~body ()
  end

(* Serve a static asset from a resource embedded with ocaml-crunch,
   with a cache of one day (by default). *)
let crunch ?content_type ?(max_age=86400) _ = object
  inherit t

  val! can_get = `Viewer

  method! private get ctx =
    let path = Context.uri ctx |> Uri.path in
    match Static.read path with
    | None -> Utils.Server.respond_not_found ()
    | Some body ->
      let content_type = Option.value ~default:(Magic_mime.lookup path) content_type in
      let headers =
        Cohttp.Header.of_list [
            ("Content-Type", content_type);
            ("Cache-Control", Printf.sprintf "public, max-age=%d;" max_age);
          ]
      in
      Utils.Server.respond_string ~status:`OK ~headers ~body ()
end
