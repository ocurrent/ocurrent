open Lwt.Infix
open Current.Syntax

(* Currently, this fires whenever we get an incoming web-hook.
   Ideally, it would be more fine-grained. *)
let webhook_cond = Lwt_condition.create ()

let input_webhook () = Lwt_condition.broadcast webhook_cond ()

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let remaining_points =
    let help = "Points remaining at time of last query" in
    Gauge.v_label ~label_name:"account" ~help ~namespace ~subsystem "remaining_points"

  let used_points_total =
    let help = "Total GraphQL query points used" in
    Counter.v_label ~label_name:"account" ~help ~namespace ~subsystem "used_points_total"
end

let graphql_endpoint = Uri.of_string "https://api.github.com/graphql"

let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    (fun () ->
       let len = in_channel_length ch in
       really_input_string ch len
    )
    ~finally:(fun () -> close_in ch)

module Repo_map = Map.Make(Repo_id)

type token = {
  token : (string, [`Msg of string]) result;
  expiry : float option;
}

let no_token = {
  token = Error (`Msg "Not fetched yet");
  expiry = Some (-1.0);
}

type t = {
  account : string;          (* Prometheus label used to report points. *)
  get_token : unit -> token Lwt.t;
  token_lock : Lwt_mutex.t;
  mutable token : token;
  mutable head_inputs : Current_git.Commit_id.t Current.Input.t Repo_map.t;
}

let v ~get_token account =
  let head_inputs = Repo_map.empty in
  let token_lock = Lwt_mutex.create () in
  { get_token; token_lock; token = no_token; head_inputs; account }

let of_oauth token =
  let get_token () = Lwt.return { token = Ok token; expiry = None } in
  v ~get_token "oauth"

let get_token t =
  Lwt_mutex.with_lock t.token_lock @@ fun () ->
  let now = Unix.gettimeofday () in
  match t.token with
  | { token; expiry = None } -> Lwt.return token
  | { token; expiry = Some expiry } when now < expiry -> Lwt.return token
  | _ ->
    Log.info (fun f -> f "Getting API token");
    Lwt.catch t.get_token
      (fun ex ->
         Log.warn (fun f -> f "Error getting GitHub token: %a" Fmt.exn ex);
         let token = Error (`Msg "Failed to get GitHub token") in
         let expiry = Some (now +. 60.0) in
         Lwt.return {token; expiry}
      )
    >|= fun token ->
    t.token <- token;
    token.token

let ( / ) a b = Yojson.Safe.Util.member b a

let exec_graphql ?variables t query =
  let body =
    `Assoc (
      ("query", `String query) ::
      (match variables with
       | None -> []
       | Some v -> ["variables", `Assoc v])
    )
    |> Yojson.Safe.to_string
    |> Cohttp_lwt.Body.of_string
  in
  get_token t >>= function
  | Error (`Msg m) -> Lwt.fail_with m
  | Ok token ->
    let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ token) in
    Cohttp_lwt_unix.Client.post ~headers ~body graphql_endpoint >>=
    fun (resp, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    match Cohttp.Response.status resp with
    | `OK ->
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      begin match json / "errors" with
        | `Null -> json
        | errors ->
          Log.warn (fun f -> f "@[<v2>GitHub returned errors: %a@]" (Yojson.Safe.pretty_print ~std:true) json);
          match errors with
          | `List (error :: _) ->
            let msg = error / "message" |> to_string in
            Fmt.failwith "Error from GitHub GraphQL: %s" msg;
          | _ ->
            Fmt.failwith "Unknown error type from GitHub GraphQL"
      end
    | err -> Fmt.failwith "@[<v2>Error performing GraphQL query on GitHub: %s@,%s@]"
               (Cohttp.Code.string_of_status err)
               body

let query_default =
  "query($owner: String!, $name: String!) { \
   rateLimit { \
     cost \
     remaining \
     resetAt \
   } \
   repository(owner: $owner, name: $name) { \
     url \n
     defaultBranchRef { \
       prefix \
       name \
       target { \
         oid \
       } \
     } \
   } \
 }"

let handle_rate_limit t name json =
  let open Yojson.Safe.Util in
  let cost = json / "cost" |> to_int in
  let remaining = json / "remaining" |> to_int in
  let reset_at = json / "resetAt" |> to_string in
  Log.info (fun f -> f "GraphQL(%s): cost:%d remaining:%d resetAt:%s" name cost remaining reset_at);
  Prometheus.Counter.inc (Metrics.used_points_total t.account) (float_of_int cost);
  Prometheus.Gauge.set (Metrics.remaining_points t.account) (float_of_int remaining)

let default_ref t { Repo_id.owner; name } =
    let variables = [
      "owner", `String owner;
      "name", `String name;
    ] in
    exec_graphql t ~variables query_default >|= fun json ->
    try
      let open Yojson.Safe.Util in
      let data = json / "data" in
      handle_rate_limit t "default_ref" (data / "rateLimit");
      let repo = data / "repository" in
      let url = repo / "url" |> to_string in
      let def = repo / "defaultBranchRef" in
      let prefix = def / "prefix" |> to_string in
      let name = def / "name" |> to_string in
      let hash = def / "target" / "oid" |> to_string in
      Current_git.Commit_id.v
        ~repo:(url ^ ".git")
        ~gref:(prefix ^ name)
        ~hash
    with ex ->
      let pp f j = Yojson.Safe.pretty_print f j in
      Log.err (fun f -> f "@[<v2>Invalid JSON: %a@,%a@]" Fmt.exn ex pp json);
      raise ex

let make_head_commit_input t repo =
  let read () =
    Lwt.catch
      (fun () -> default_ref t repo >|= Stdlib.Result.ok)
      (fun ex -> Lwt_result.fail @@ `Msg (Fmt.strf "GitHub query for %a failed: %a" Repo_id.pp repo Fmt.exn ex))
  in
  let watch refresh =
    let rec aux x =
      x >>= fun () ->
      let x = Lwt_condition.wait webhook_cond in
      refresh ();
      Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
      aux x
    in
    let x = Lwt_condition.wait webhook_cond in
    let thread =
      Lwt.catch
        (fun () -> aux x)
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | ex -> Log.err (fun f -> f "head_commit thread failed: %a" Fmt.exn ex); Lwt.return_unit
        )
    in
    Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit)
  in
  let pp f = Fmt.pf f "Watch %a default ref head" Repo_id.pp repo in
  Current.monitor ~read ~watch ~pp

let head_commit t repo =
  Current.component "%a head" Repo_id.pp repo |>
  let> () = Current.return () in
  match Repo_map.find_opt repo t.head_inputs with
  | Some i -> i
  | None ->
    let i = make_head_commit_input t repo in
    t.head_inputs <- Repo_map.add repo i t.head_inputs;
    i

let head_commit_dyn t repo =
  Current.component "head" |>
  let> t = t
  and> repo = repo in
  match Repo_map.find_opt repo t.head_inputs with
  | Some i -> i
  | None ->
    let i = make_head_commit_input t repo in
    t.head_inputs <- Repo_map.add repo i t.head_inputs;
    i

open Cmdliner

let token_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitHub OAuth token."
    ~docv:"PATH"
    ["github-token-file"]

let make_config token_file =
  of_oauth @@ (String.trim (read_file token_file))

let cmdliner =
  Term.(const make_config $ token_file)
