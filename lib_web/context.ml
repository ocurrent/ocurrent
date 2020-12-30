open Lwt.Infix

let cookie_key = "__session"

type t = {
  site : Site.t;
  session : Site.Sess.t;
  user : User.t option;
  request : Cohttp.Request.t;
}

let of_request ~site request =
  let headers = Cohttp.Request.headers request in
  Site.Sess.of_header_or_create site.Site.session_backend cookie_key "" headers >>= fun session ->
  begin
    match User.unmarshal session.value with
    | Ok x -> Lwt.return x
    | Error m ->
      Log.err (fun f -> f "Invalid user in session table: %s" m);
      Site.Sess.clear site.session_backend session >|= fun () ->
      None
  end >|= fun user ->
  { site; session; user; request }

let headers t =
  Site.Sess.to_cookie_hdrs cookie_key t.session
    ~path:"/"
    ~secure:t.site.secure_cookies
  |> Cohttp.Header.of_list

(* Just use the hash of the session key as the CSRF token.
   Perhaps we could use the key itself, but this seems slightly safer. *)
let csrf t =
  t.session.key
  |> Cstruct.of_string
  |> Mirage_crypto.Hash.SHA256.digest
  |> Cstruct.to_string
  |> Base64.(encode_exn ~alphabet:uri_safe_alphabet)

let has_role t role =
  let ok = t.site.has_role t.user role in
  if not ok then
    Log.info (fun f -> f "%a does not have required role %a"
                 (Fmt.option ~none:(Fmt.any "(anonymous)") User.pp) t.user Role.pp role);
  ok

let request t = t.request

let uri t = Cohttp.Request.uri (request t)

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let logout_form t user =
  let link_path = "/logout" in
  let link_label = Fmt.str "Log out %s" (User.id user) in
  let open Tyxml.Html in
  [
    li [
      form ~a:[a_action link_path; a_method `Post] [
        button [txt link_label];
        input ~a:[a_name "csrf"; a_input_type `Hidden; a_value (csrf t)] ();
      ]
    ]
  ]

let render_nav_link (link_label, path) =
  let open Tyxml.Html in
  li [a ~a:[a_href path] [txt link_label]]

let template t contents =
  let site = t.site in
  let open Tyxml.Html in
  html_to_string (
    html
      (head (title (txt site.name)) [
          link ~rel:[ `Stylesheet ] ~href:"/css/style.css" ();
          meta ~a:[a_charset "UTF-8"] ();
        ]
      )
      (body [
          nav [
            ul (
              li [a ~a:[a_href "/"] [txt site.name]] ::
              li [a ~a:[a_href "/"] [txt "Home"]] ::
              List.map render_nav_link site.nav_links
            );
            ul ~a:[a_class ["right"]] (
              match t.site.authn with
              | None -> []
              | Some login_uri ->
                match t.user with
                | None ->
                  let uri = login_uri ~csrf:(csrf t) in
                  [li [a ~a:[a_href (Uri.to_string uri)] [txt "Log in"]]]
                | Some user -> logout_form t user
            )
          ];
          div ~a:[a_id "main"] contents
        ]
      )
  )

let respond_ok t body =
  let body = template t body in
  Utils.Server.respond_string ~headers:(headers t) ~status:`OK ~body ()

let respond_redirect t uri =
  Utils.Server.respond_redirect ~headers:(headers t) ~uri ()

let respond_error t status msg =
  let body = template t [Tyxml.Html.txt msg] in
  Utils.Server.respond_string ~headers:(headers t) ~status ~body ()

let set_user t user =
  Site.Sess.generate t.site.session_backend (User.marshal user) >>= fun session ->
  respond_redirect { t with session } (Uri.of_string "/")
