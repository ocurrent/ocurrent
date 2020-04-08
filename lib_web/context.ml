type t = {
  site : Site.t;
  request : Cohttp.Request.t;
}

let of_request ~site request =
  Lwt.return { site; request }

let request t = t.request

let uri t = Cohttp.Request.uri (request t)

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

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
            ul [
              li [a ~a:[a_href "/"] [txt site.name]];
              li [a ~a:[a_href "/"] [txt "Home"]];
              li [a ~a:[a_href "/jobs"] [txt "Jobs"]];
              li [a ~a:[a_href "/query"] [txt "Query"]];
              li [a ~a:[a_href "/log-rules"] [txt "Log analysis"]];
            ];
          ];
          div ~a:[a_id "main"] contents
        ]
      )
  )

let respond_ok t body =
  let body = template t body in
  Utils.Server.respond_string ~status:`OK ~body ()

let respond_redirect _t uri =
  Utils.Server.respond_redirect ~uri ()

let respond_error t status msg =
  let body = template t [Tyxml.Html.txt msg] in
  Utils.Server.respond_string ~status ~body ()
