open Lwt.Infix

module Server = Cohttp_lwt_unix.Server

type t = {
  client_id : string;
  client_secret: string;
  scopes : string list;
} [@@deriving yojson]

let v ?(scopes=["user:email"]) ~client_id ~client_secret () =
  { client_id; client_secret; scopes }

module Endpoint = struct
  let authorize =
    let uri = Uri.of_string "https://github.com/login/oauth/authorize" in
    fun ~scopes ~state t ->
      let scopes = String.concat " " scopes in
      Uri.with_query' uri [
        "scope", scopes;
        "client_id", t.client_id;
        "state", state;
      ]

  let access_token =
    let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
    fun t ~state code ->
      Uri.with_query' uri [
        "client_id", t.client_id;
        "client_secret", t.client_secret;
        "code", code;
        "state", state;
      ]

  let user = Uri.of_string "https://api.github.com/user"
end

let make_login_uri t ~csrf =
  Endpoint.authorize ~scopes:t.scopes ~state:csrf t

let get_access_token t ~state code =
  let headers = Cohttp.Header.init_with "Accept" "application/json" in
  Cohttp_lwt_unix.Client.post ~headers (Endpoint.access_token t ~state code) >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Response.status resp with
  | `OK ->
    let json = Yojson.Safe.from_string body in
    Ok (Yojson.Safe.Util.(json |> member "access_token" |> to_string))
  | err -> Error (err, body)

let get_user token =
  let headers = Cohttp.Header.init_with "Authorization" ("token " ^ token) in
  Cohttp_lwt_unix.Client.get ~headers Endpoint.user >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Response.status resp with
  | `OK ->
    let json = Yojson.Safe.from_string body in
    let github_user = Yojson.Safe.Util.(json |> member "login" |> to_string) in
    Ok ("github:" ^ github_user)
  | err ->
    Error (err, body)

let example_config () =
  v ~client_id:"..." ~client_secret:"..." ()
  |> to_yojson
  |> Yojson.Safe.pretty_to_string

let configuration_howto ctx =
  Current_web.Context.respond_ok ctx Tyxml.Html.[
      p [ txt "GitHub single-sign-on has not been configured." ];
      p [
        txt "Start the service with ";
        code [txt "--github-oauth path.json"];
        txt ", where the file contains:";
      ];
      pre [ txt (example_config ()) ]
    ]

let login t : Current_web.Resource.t = object
  method get_raw site request =
    Current_web.Context.of_request ~site request >>= fun ctx ->
    match t with
    | None -> configuration_howto ctx
    | Some t ->
      let uri = Cohttp.Request.uri request in
      match Uri.get_query_param uri "code", Uri.get_query_param uri "state" with
      | None, _ -> Server.respond_error ~status:`Bad_request ~body:"Missing code" ()
      | _, None -> Server.respond_error ~status:`Bad_request ~body:"Missing state" ()
      | Some code, Some state ->
        if state <> Current_web.Context.csrf ctx then (
          Server.respond_error ~status:`Bad_request ~body:"Bad CSRF token" ()
        ) else (
          get_access_token t ~state code >>= function
          | Error (status, msg) ->
            Log.warn (fun f -> f "Failed to get OAuth token from GitHub: %s: %s" (Cohttp.Code.string_of_status status) msg);
            Server.respond_error ~status:`Internal_server_error ~body:"Failed to get token" ()
          | Ok token ->
            get_user token >>= function
            | Error (status, msg) ->
              Log.warn (fun f -> f "Failed to get user details from GitHub: %s: %s" (Cohttp.Code.string_of_status status) msg);
              Server.respond_error ~status:`Internal_server_error ~body:"Failed to get user details" ()
            | Ok user ->
              Log.info (fun f -> f "Successful login for %S" user);
              match Current_web.User.v user with
              | Error (`Msg m) ->
                Log.warn (fun f -> f "Failed to create user: %s" m);
                Server.respond_error ~status:`Bad_request ~body:"Bad user" ()
              | Ok user ->
                Current_web.Context.set_user ctx user
        )

  method post_raw _ _ _ =
    Server.respond_error ~status:`Bad_request ~body:"Bad method" ()

  method nav_link = None
end

open Cmdliner

let oauth_config =
  Arg.value @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The JSON file containing the GitHub OAuth configuration"
    ~docv:"PATH"
    ["github-oauth"]

let make_config path =
  match Yojson.Safe.from_file path with
  | exception ex -> Fmt.failwith "Invalid JSON in %s:@,%a" path Fmt.exn ex
  | json ->
    json
    |> of_yojson
    |> function
    | Ok x -> x
    | Error msg ->
      Fmt.failwith "Invalid GitHub OAuth configuration: %s@.Expected: %s" msg (example_config ())

let cmdliner =
  Term.(const (Option.map make_config) $ oauth_config)
