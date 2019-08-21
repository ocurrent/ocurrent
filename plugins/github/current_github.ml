open Current.Syntax
open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let remaining_points =
    let help = "Points remaining at time of last query" in
    Gauge.v ~help ~namespace ~subsystem "remaining_points"

  let used_points_total =
    let help = "Total GraphQL query points used" in
    Counter.v ~help ~namespace ~subsystem "used_points_total"
end

let endpoint = Uri.of_string "https://api.github.com/graphql"

let src = Logs.Src.create "current.github" ~doc:"OCurrent GitHub plugin"
module Log = (val Logs.src_log src : Logs.LOG)

(* Currently, this fires whenever we get an incoming web-hook.
   Ideally, it would be more fine-grained. *)
let webhook_cond = Lwt_condition.create ()

module Repo_id = struct
  type t = {
    owner : string;
    name : string;
  }

  let pp f { owner; name } = Fmt.pf f "%s/%s" owner name

  let compare = compare

  let cmdliner =
    let open Cmdliner in
    let parse s =
      match Astring.String.cuts ~sep:"/" s with
      | [ owner; name] -> Ok { owner; name }
      | _ -> Error (`Msg (Fmt.strf "%S not in the form 'owner/name'" s))
    in
    Arg.conv ~docv:"REPO" (parse, pp)
end

module Repo_map = Map.Make(Repo_id)

type t = {
  token : string;
  mutable head_inputs : Current_git.Commit_id.t Current.Input.t Repo_map.t;
}

let v ~token () =
  let head_inputs = Repo_map.empty in
  { token; head_inputs }

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
  let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ t.token) in
  Cohttp_lwt_unix.Client.post ~headers ~body endpoint >>=
  fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Response.status resp with
  | `OK -> Yojson.Safe.from_string body
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

let ( / ) a b = Yojson.Safe.Util.member b a

let handle_rate_limit name json =
  let open Yojson.Safe.Util in
  let cost = json / "cost" |> to_int in
  let remaining = json / "remaining" |> to_int in
  let reset_at = json / "resetAt" |> to_string in
  Log.info (fun f -> f "GraphQL(%s): cost:%d remaining:%d resetAt:%s" name cost remaining reset_at);
  Prometheus.Counter.inc Metrics.used_points_total (float_of_int cost);
  Prometheus.Gauge.set Metrics.remaining_points (float_of_int remaining)

let default_ref t { Repo_id.owner; name } =
    let variables = [
      "owner", `String owner;
      "name", `String name;
    ] in
    exec_graphql t ~variables query_default >|= fun json ->
    try
      let open Yojson.Safe.Util in
      let data = json / "data" in
      handle_rate_limit "default_ref" (data / "rateLimit");
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

let input_webhook req _body =
  Log.info (fun f -> f "input_webhook: %a" Cohttp_lwt.Request.pp_hum req);
  Lwt_condition.broadcast webhook_cond ();
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()

let make_config token_file =
  let ch = open_in_bin token_file in
  let token =
    String.trim @@
    Fun.protect
      (fun () -> input_line ch)
      ~finally:(fun () -> close_in ch)
  in
  v ~token ()

open Cmdliner

let token_file =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"A file containing the GitHub OAuth token."
    ~docv:"PATH"
    ["github-token-file"]

let cmdliner =
  Term.(const make_config $ token_file)
