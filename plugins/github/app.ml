open Current.Syntax
open Lwt.Infix

let ( >>!= ) = Lwt_result.bind

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let installations_total =
    let help = "Total number of active app installations" in
    Gauge.v ~help ~namespace ~subsystem "installations_total"
end


let installations_changed_cond = Lwt_condition.create ()    (* Fires when the list should be updated *)

let input_installation_webhook () = Lwt_condition.broadcast installations_changed_cond ()

let list_installations_endpoint =
  Uri.of_string "https://api.github.com/app/installations"

let access_tokens_endpoint id =
  Uri.of_string (Fmt.str "https://api.github.com/app/installations/%d/access_tokens" id)

module Int_map = Map.Make(Int)

module Installs = Current.Var(struct
    type t = Installation.t Int_map.t
    let equal = Int_map.equal (fun _ _ -> true)
    let pp = Fmt.using (fun t -> Int_map.bindings t |> List.map fst) Fmt.(Dump.list int)
  end)

module Allowlist : sig

  type t

  val of_list : string list -> t

  val mem : string -> t -> bool

end = struct
  type t = string list
  let of_list = List.map String.lowercase_ascii
  let mem name l = List.exists ((=) (String.lowercase_ascii name)) l
end

type t = {
  app_id : string;
  key : Mirage_crypto_pk.Rsa.priv;
  allowlist : Allowlist.t;      (* Accounts which can use this app. *)
  installations : Installs.t;
}

let http { app_id; key; _ } op uri =
  let iat = truncate @@ Unix.gettimeofday () in
  let jwt = Token.encode ~key ~iat ~app_id in
  let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ jwt) in
  let headers = Cohttp.Header.add headers "accept" "application/vnd.github.machine-man-preview+json" in
  Log.debug (fun f -> f "API call on %a" Uri.pp uri);
  op ~headers uri >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Response.status resp with
  | `OK | `Created ->
    let json = Yojson.Safe.from_string body in
    Log.debug (fun f -> f "@[<v2>Got response:@,%a@]" Yojson.Safe.pp json);
    resp, json
  | err -> Fmt.failwith "@[<v2>Error accessing GitHub App API at %a: %s@,%s@]"
             Uri.pp uri
             (Cohttp.Code.string_of_status err)
             body

let get ~headers uri = Cohttp_lwt_unix.Client.get ~headers uri
let post ~headers uri = Cohttp_lwt_unix.Client.post ~headers uri

let minute = 60.0

let get_token app iid =
  let uri = access_tokens_endpoint iid in
  let now = Unix.gettimeofday () in
  http app post uri >|= fun (_resp, json) ->
  let open Yojson.Safe.Util in
  let token = Ok (json |> member "token" |> to_string) in
  (* The token is valid for 60 minutes, so request a new one after 50 minutes. *)
  let expiry = Some (now +. 50.0 *. minute) in
  Api.{ token; expiry }

let next headers =
  headers
  |> Cohttp.Header.get_links
  |> List.find_opt (fun (link : Cohttp.Link.t) ->
      List.exists (fun r -> r = Cohttp.Link.Rel.next) link.arc.relation
    )
  |> Option.map (fun link -> link.Cohttp.Link.target)

let get_installations app =
  Lwt.catch (fun () ->
     let rec aux uri =
       http app get uri >>= fun (resp, json) ->
       let open Yojson.Safe.Util in
       let installs =
         json |> to_list |> List.filter_map (fun json ->
             let id = json |> member "id" |> to_int in
             let account = json |> member "account" |> member "login" |> to_string in
             if Allowlist.mem account app.allowlist then (
               Log.info (fun f -> f "Found installation %d for %S" id account);
               let repository_selection = json |> member "repository_selection" |> to_string in
               match repository_selection with
               | "selected" -> Some (id, account)
               | "all" ->
                 Log.warn (fun f -> f "Installation %S has selected all repositories - skipping as probably a mistake" account);
                 None
               | x ->
                 Log.warn (fun f -> f "Installation %S has unknown repository_selection %S - skipping" account x);
                 None
             ) else (
               Log.warn (fun f -> f "Installation %d for %S : account not on allowlist!" id account);
               None
             )
           )
       in
       match next (Cohttp.Response.headers resp) with
       | None -> Lwt_result.return installs
       | Some target ->
         aux target >>!= fun next_installs ->
         Lwt_result.return (installs @ next_installs)
     in
     aux list_installations_endpoint
    ) (fun ex ->
      Lwt_result.fail (`Msg (Fmt.str "Failed to get GitHub installations: %a" Fmt.exn ex))
    )

let installation t ~account iid =
  let api = Api.v ~get_token:(fun () -> get_token t iid) ("i-" ^ account) ~app_id:t.app_id in
  Installation.v ~api ~account ~iid

module Int_set = Set.Make(Int)

let remove_stale_installations new_ids =
  let new_ids = new_ids |> List.map fst |> Int_set.of_list in
  Int_map.filter (fun key _ -> Int_set.mem key new_ids)

let monitor_installations t () =
  let rec aux () =
    let update = Lwt_condition.wait installations_changed_cond in
    get_installations t >>= fun ids ->
    begin match ids with
      | Ok ids ->
        Prometheus.Gauge.set Metrics.installations_total (float_of_int (List.length ids));
        Installs.update t.installations (fun old_map ->
            let old_map = match old_map with Ok x -> x | Error _ -> Int_map.empty in
            (* Merge in new installations. Reuse existing Apis so we don't keep refreshing tokens, etc. *)
            ids |> ListLabels.fold_left ~init:old_map ~f:(fun acc (iid, account) ->
                if Int_map.mem iid acc then acc
                else Int_map.add iid (installation t iid ~account) acc
              )
            |> remove_stale_installations ids
            |> Stdlib.Result.ok
          );
      | Error (`Msg m) ->
        Log.warn (fun f -> f "Failed to update list of installations: %s" m)
    end;
    Lwt_unix.sleep 60.0 >>= fun () ->   (* Wait at least 1m between updates *)
    update >>= aux
  in
  aux ()

let installations t =
  let+ apis = Installs.get t.installations in
  apis |> Int_map.bindings |> List.map snd

(* Command-line options *)

let make_config app_id private_key_file allowlist =
  let allowlist = Allowlist.of_list allowlist in
  let data = Api.read_file private_key_file in
  match X509.Private_key.decode_pem (Cstruct.of_string data) with
    | Error (`Msg msg) -> Fmt.failwith "Failed to parse secret key!@ %s" msg
    | Ok (`RSA key) ->
      let installations = Installs.create ~name:"installations" (Error (`Active `Running)) in
      let t = { app_id; key; allowlist; installations } in
      Lwt.async (monitor_installations t);
      t
    | Ok _ -> Fmt.failwith "Unsupported private key type" [@@warning "-11"]

open Cmdliner

let make_config_opt app_id private_key_file allowlist : t option Term.ret =
  match app_id, private_key_file, allowlist with
  | None, None, _ -> `Ok None
  | Some app_id, Some private_key_file, Some allowlist -> `Ok (Some (make_config app_id private_key_file allowlist))
  | Some _, Some _, None -> `Error (true, "--github-account-allowlist is required with --github-app-id")
  | Some _, None, _ -> `Error (true, "--github-private-key-file is required with --github-app-id")
  | None, Some _, _ -> `Error (true, "--github-app-id is required with --github-private-key-file")

let private_key_file =
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitHub app's RSA private key."
    ~docv:"PATH"
    ["github-private-key-file"]

let app_id =
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"The GitHub app's (integer) ID"
    ~docv:"ID"
    ["github-app-id"]

let allowlist =
  Arg.opt Arg.(some (list string)) None @@
  Arg.info
    ~doc:"A comma-separated list of allowed GitHub accounts"
    ~docv:"ACCOUNTS"
    ["github-account-allowlist"]

let cmdliner =
  Term.(const make_config $ Arg.required app_id $ Arg.required private_key_file $ Arg.required allowlist)

let cmdliner_opt =
  Term.(ret (const make_config_opt $ Arg.value app_id $ Arg.value private_key_file $ Arg.value allowlist))
