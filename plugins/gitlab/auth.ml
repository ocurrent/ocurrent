open Lwt.Infix

module Server = Cohttp_lwt_unix.Server

type t = {
  client_id : string;
  client_secret: string;
  scopes : string list;
  redirect_uri : string;
} [@@deriving yojson]

let v ?(scopes=["read_user"]) ~client_id ~client_secret ~redirect_uri () =
  { client_id; client_secret; scopes; redirect_uri }

exception ScopeOfString of string

let scope_of_string scope =
  match Gitlab.Scope.of_string scope with
  | Some s -> s
  | None -> raise @@ ScopeOfString ("Invalid option for scope_of_string :" ^ scope)

let make_login_uri t ~csrf =
  Gitlab.Token.create_url
    ~client_id: t.client_id
    ~redirect_uri:(Uri.of_string t.redirect_uri)
    ~state:csrf
    ~scopes: (List.map scope_of_string t.scopes)
    ()

let get_access_token t code =
  Gitlab.Token.of_code
    ~client_id: t.client_id
    ~code
    ~client_secret: t.client_secret
    ~redirect_uri: t.redirect_uri
    ()

let get_user token =
  let open Gitlab in
  let open Monad in
  let cmd =
    User.current_user ~token () >|~ fun user ->
    Ok ("gitlab:" ^ user.Gitlab_t.user_username)
  in
  run cmd

let example_config () =
  v ~client_id:"…" ~client_secret:"…" ~redirect_uri:"…" ()
  |> to_yojson
  |> Yojson.Safe.pretty_to_string

let configuration_howto ctx =
  Current_web.Context.respond_ok ctx Tyxml.Html.[
      p [ txt "GitLab single-sign-on has not been configured." ];
      p [
        txt "Start the service with ";
        code [txt "--gitlab-oauth path.json"];
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
          get_access_token t code >>= function
          | None ->
            Server.respond_error ~status:`Internal_server_error ~body:"Failed to get token" ()
          | Some token ->
            get_user token >>= function
            | Error (status, msg) ->
              Log.warn (fun f -> f "Failed to get user details from GitLab: %s: %s" (Cohttp.Code.string_of_status status) msg);
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
    ~doc:"The JSON file containing the GitLab OAuth configuration"
    ~docv:"PATH"
    ["gitlab-oauth"]

let make_config path =
  match Yojson.Safe.from_file path with
  | exception ex -> Fmt.failwith "Invalid JSON in %s:@,%a" path Fmt.exn ex
  | json ->
    json
    |> of_yojson
    |> function
    | Ok x -> x
    | Error msg ->
      Fmt.failwith "Invalid GitLab OAuth configuration: %s@.Expected: %s" msg (example_config ())

let cmdliner =
  Term.(const (Option.map make_config) $ oauth_config)
