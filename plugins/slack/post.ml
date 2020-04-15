open Lwt.Infix

type t = Uri.t

let id = "slack-post"

module Key = Current.String
module Value = Current.String
module Outcome = Current.Unit

(* Slack only allows EC crypto, which ocaml-tls doesn't support yet, so force the use of OpenSSL. *)
module Net = struct
  include Cohttp_lwt_unix.Net

  let force_openssl = function
    | `TLS x | `TLS_native x | `OpenSSL x -> `OpenSSL x
    | _ -> failwith "force_openssl: unexpected Slack connection type!"

  let connect_uri ~ctx uri =
    Resolver_lwt.resolve_uri ~uri ctx.resolver
    >>= fun endp ->
    Conduit_lwt_unix.endp_to_client ~ctx:ctx.ctx endp
    >>= fun client ->
    Conduit_lwt_unix.connect ~ctx:ctx.ctx (force_openssl client)
end

module Client = Cohttp_lwt.Make_client(Cohttp_lwt_unix.IO)(Net)

let publish t job _key message =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  let headers = Cohttp.Header.of_list [
      "Content-type", "application/json";
    ]
  in
  let body = `Assoc [
      "text", `String message;
    ]
    |> Yojson.to_string
    |> Cohttp_lwt.Body.of_string
  in
  Client.post ~headers ~body t >>= fun (resp, _body) ->
  match resp.Cohttp_lwt.Response.status with
  | `OK -> Lwt.return @@ Ok ()
  | err ->
    let msg = Fmt.strf "Slack post failed: %s" (Cohttp.Code.string_of_status err) in
    Lwt.return @@ Error (`Msg msg)

let pp f (key, value) = Fmt.pf f "Post %s: %s" key value

let auto_cancel = false
