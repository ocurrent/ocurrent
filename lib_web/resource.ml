open Lwt.Infix

let () = Mirage_crypto_rng_unix.initialize ()

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

  method get_raw site request =
    Context.of_request ~site request >>= fun ctx ->
    if Context.has_role ctx can_get then self#get ctx
    else forbidden ctx

  method post_raw site request body =
    Context.of_request ~site request >>= fun ctx ->
    if Context.has_role ctx can_post then (
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
