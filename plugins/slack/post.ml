type channel = Uri.t

type t = channel * Eio.Net.t

let id = "slack-post"

let tls_config =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  let null ?ip:_ ~host:_ _certs = Ok None in
  Tls.Config.client ~authenticator:null ()

module Key = Current.String
module Value = Current.String
module Outcome = Current.Unit

let (>>?=) f v = Result.bind f v

let host_and_path uri =
  match Uri.host uri, Uri.path uri with
  | Some host, path -> Ok (host, path)
  | _ -> Error (`Msg ("Failed to extract host from " ^ Uri.to_string uri))

let publish (uri, net) job _key message =
  let open Eio in
  Current.Job.start job ~level:Current.Level.Above_average;
  let headers = Http.Header.of_list [
      "Content-type", "application/json";
    ]
  in
  let body = `Assoc [
      "text", `String message;
    ]
    |> Yojson.to_string
    |> fun s -> Cohttp_eio.Body.Fixed s
  in
  host_and_path uri >>?= fun (host, path) ->
  match Net.getaddrinfo_stream ~service:"https" net host with
  | [] -> Error (`Msg "Host resolution failed")
  | stream :: _ ->
    Switch.run @@ fun sw ->
    let conn = Net.connect ~sw net stream in
    let conn =
      Tls_eio.client_of_flow tls_config
        ?host:
          (Domain_name.of_string_exn host
          |> Domain_name.host |> Result.to_option)
        conn
    in
    (* TODO: The cohttp-eio needs to be made a little more nicer to use... *)
    let resp, _body = Cohttp_eio.Client.post ~headers ~body ~conn ~host (object method net = net end) path in
    match resp.Http.Response.status with
    | `OK -> Ok ()
    | err ->
      let msg = Fmt.str "Slack post failed: %s" (Http.Status.to_string err) in
      Error (`Msg msg)

let pp f (key, value) = Fmt.pf f "Post %s: %s" key value

let auto_cancel = false
