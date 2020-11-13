open Lwt.Infix
open Current.Syntax

(* Limit updates to one at a time for now. *)
let pool = Current.Pool.create ~label:"github" 1

(* When we get a webhook event, we fire the condition for that repository `owner/name`, if any. *)
let webhook_cond = Hashtbl.create 10

let await_event ~owner_name =
  let cond =
    match Hashtbl.find_opt webhook_cond owner_name with
    | Some c -> c
    | None ->
      let c = Lwt_condition.create () in
      Hashtbl.add webhook_cond owner_name c;
      c
  in
  Lwt_condition.wait cond

let input_webhook body =
  let owner_name = Yojson.Safe.Util.(body |> member "repository" |> member "full_name" |> to_string) in
  match Hashtbl.find_opt webhook_cond owner_name with
  | Some cond -> Lwt_condition.broadcast cond ()
  | None -> Log.info (fun f -> f "Got webhook event for %S, but we're not interested in that" owner_name)

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

  let refs_total =
    let help = "Total number of monitored branches" in
    Gauge.v_labels ~label_names:["account"; "name"] ~help ~namespace ~subsystem "refs_total"

  let prs_total =
    let help = "Total number of monitored PRs" in
    Gauge.v_labels ~label_names:["account"; "name"] ~help ~namespace ~subsystem "prs_total"
end

let graphql_endpoint = Uri.of_string "https://api.github.com/graphql"

let status_endpoint ~owner_name ~commit =
  Uri.of_string (Fmt.str "https://api.github.com/repos/%s/statuses/%s"
                   owner_name commit)

let read_file path =
  let ch = open_in_bin path in
  Fun.protect
    (fun () ->
       let len = in_channel_length ch in
       really_input_string ch len
    )
    ~finally:(fun () -> close_in ch)

module Repo_map = Map.Make(Repo_id)

module Status = struct
  type state = [`Error | `Failure | `Pending | `Success ]

  type t = {
    state : state;
    description : string option;
    url : Uri.t option;
  }

  let v ?description ?url state =
    let description = Option.map (Astring.String.with_range ~len:140) description in (* Max GitHub allows *)
    { state; description; url }

  let state_to_string = function
    | `Error   -> "error"
    | `Failure -> "failure"
    | `Pending -> "pending"
    | `Success -> "success"

  let json_items { state; description; url } =
    ["state", `String (state_to_string state)] @
    (match description with None -> [] | Some x -> ["description", `String x]) @
    (match url with None -> [] | Some x -> ["target_url", `String (Uri.to_string x)])

  let digest t = Yojson.Safe.to_string @@ `Assoc (json_items t)

  let pp f t = Fmt.string f (digest t)
end

type token = {
  token : (string, [`Msg of string]) result;
  expiry : float option;
}

let no_token = {
  token = Error (`Msg "Not fetched yet");
  expiry = Some (-1.0);
}

module Ref = struct
  type t = [ `Ref of string | `PR of int ] [@@deriving to_yojson]

  let compare = Stdlib.compare

  let pp f = function
    | `Ref r -> Fmt.string f r
    | `PR pr -> Fmt.pf f "PR %d" pr

  let to_git = function
    | `Ref head -> head
    | `PR id -> Fmt.str "refs/pull/%d/head" id
end

module Ref_map = Map.Make(Ref)

module Commit_id = struct
  type t = {
    owner_name : string;    (* e.g. "owner/name" *)
    id : Ref.t;
    hash : string;
    committed_date : string;
  } [@@deriving to_yojson]

  let to_git { owner_name; id; hash; _ } =
    let repo = Fmt.str "https://github.com/%s.git" owner_name in
    let gref = Ref.to_git id in
    Current_git.Commit_id.v ~repo ~gref ~hash

  let uri t =
    Uri.make ~scheme:"https" ~host:"github.com" ~path:(Printf.sprintf "/%s/commit/%s" t.owner_name t.hash) ()

  let pp_id = Ref.pp

  let compare {owner_name; id; hash; _} b =
    match compare hash b.hash with
    | 0 ->
      begin match Ref.compare id b.id with
        | 0 -> compare owner_name b.owner_name
        | x -> x
      end
    | x -> x

  let pp f { owner_name; id; hash; committed_date } =
    Fmt.pf f "%s@ %a@ %s@ %s" owner_name pp_id id (Astring.String.with_range ~len:8 hash) committed_date

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

type t = {
  account : string;          (* Prometheus label used to report points. *)
  get_token : unit -> token Lwt.t;
  token_lock : Lwt_mutex.t;
  mutable token : token;
  mutable head_monitors : commit Current.Monitor.t Repo_map.t;
  mutable ci_refs_monitors : ci_refs Current.Monitor.t Repo_map.t;
}
and commit = t * Commit_id.t
and ci_refs = {
  default_ref: string;
  all_refs: commit Ref_map.t
}

let get_default_ref t = t.default_ref 

let get_all_refs t = t.all_refs

let v ~get_token account =
  let head_monitors = Repo_map.empty in
  let ci_refs_monitors = Repo_map.empty in
  let token_lock = Lwt_mutex.create () in
  { get_token; token_lock; token = no_token; head_monitors; ci_refs_monitors; account }

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
    | err ->
      Log.warn (fun f -> f "@[<v2>Error performing GraphQL query on GitHub: %s@,%s@]"
                   (Cohttp.Code.string_of_status err)
                   body);
      Fmt.failwith "Error performing GraphQL query on GitHub: %s" (Cohttp.Code.string_of_status err)

let query_default =
  "query($owner: String!, $name: String!) { \
   rateLimit { \
     cost \
     remaining \
     resetAt \
   } \
   repository(owner: $owner, name: $name) { \
     nameWithOwner \n
     defaultBranchRef { \
       prefix \
       name \
       target { \
         ...on Commit { \
           oid \
           committedDate \
         } \
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
      let owner_name = repo / "nameWithOwner" |> to_string in
      let def = repo / "defaultBranchRef" in
      let prefix = def / "prefix" |> to_string in
      let name = def / "name" |> to_string in
      let hash = def / "target" / "oid" |> to_string in
      let committed_date = def / "target" / "committedDate" |> to_string in 
      { Commit_id.owner_name; id = `Ref (prefix ^ name); hash; committed_date }
    with ex ->
      let pp f j = Yojson.Safe.pretty_print f j in
      Log.err (fun f -> f "@[<v2>Invalid JSON: %a@,%a@]" Fmt.exn ex pp json);
      raise ex

let make_head_commit_monitor t repo =
  let read () =
    Lwt.catch
      (fun () -> default_ref t repo >|= fun c -> Ok (t, c))
      (fun ex -> Lwt_result.fail @@ `Msg (Fmt.str "GitHub query for %a failed: %a" Repo_id.pp repo Fmt.exn ex))
  in
  let watch refresh =
    let owner_name = Printf.sprintf "%s/%s" repo.owner repo.name in
    let rec aux x =
      x >>= fun () ->
      let x = await_event ~owner_name in
      refresh ();
      Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
      aux x
    in
    let x = await_event ~owner_name in
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
  Current.Monitor.create ~read ~watch ~pp

let head_commit t repo =
  Current.component "%a head" Repo_id.pp repo |>
  let> () = Current.return () in
  let monitor =
    match Repo_map.find_opt repo t.head_monitors with
    | Some i -> i
    | None ->
      let i = make_head_commit_monitor t repo in
      t.head_monitors <- Repo_map.add repo i t.head_monitors;
      i
  in
  Current.Monitor.get monitor

let query_branches_and_open_prs = {|
  query($owner: String!, $name: String!) {
    rateLimit {
      cost
      remaining
      resetAt
    }
    repository(owner: $owner, name: $name) {
      nameWithOwner
      defaultBranchRef {
        name
      }
      refs(first: 100, refPrefix:"refs/heads/") {
        totalCount
        edges {
          node {
            name
            target {
              ...on Commit {
                oid
                committedDate
              }
            }
          }
        }
      }
      pullRequests(first: 100, states:[OPEN]) {
        totalCount
        edges {
          node {
            number
            headRefOid
            commits(last: 1) {
              nodes {
                commit {
                  committedDate
                }
              } 
            }
          }
        }
      }
    }
  }
|}

let parse_ref ~owner_name ~prefix json =
  let open Yojson.Safe.Util in
  let node = json / "node" in
  let name = node / "name" |> to_string in
  let hash = node / "target" / "oid" |> to_string in
  let committed_date = node / "target" / "committedDate" |> to_string in
  { Commit_id.owner_name; id = `Ref (prefix ^ name); hash; committed_date }

let parse_pr ~owner_name json =
  let open Yojson.Safe.Util in
  let node = json / "node" in
  let hash = node / "headRefOid" |> to_string in
  let pr = node / "number" |> to_int in
  let nodes = node / "commits" / "nodes" |> to_list in 
  if List.length nodes = 0 then Fmt.failwith "Failed to get latest commit for %s" owner_name else 
  let committed_date = List.hd nodes / "commit" / "committedDate" |> to_string in
  { Commit_id.owner_name; id = `PR pr; hash; committed_date }

let get_ci_refs t { Repo_id.owner; name } =
    let variables = [
      "owner", `String owner;
      "name", `String name;
    ] in
    exec_graphql t ~variables query_branches_and_open_prs >|= fun json ->
    try
      let open Yojson.Safe.Util in
      let data = json / "data" in
      handle_rate_limit t "default_ref" (data / "rateLimit");
      let repo = data / "repository" in
      let owner_name = repo / "nameWithOwner" |> to_string in
      let default_ref = repo / "defaultBranchRef" / "name" |> to_string |> ( ^ ) "refs/heads/" in 
      let refs =
        repo / "refs" / "edges" |> to_list |> List.map (parse_ref ~owner_name ~prefix:"refs/heads/") in
      let prs =
        repo / "pullRequests" / "edges" |> to_list |> List.map (parse_pr ~owner_name) in
      (* TODO: use cursors to get all results.
         For now, we just take the first 100 and warn if there are more. *)
      let n_branches = repo / "refs" / "totalCount" |> to_int in
      let n_prs = repo / "pullRequests" / "totalCount" |> to_int in
      Prometheus.Gauge.set (Prometheus.Gauge.labels Metrics.refs_total [owner; name]) (float_of_int n_branches);
      Prometheus.Gauge.set (Prometheus.Gauge.labels Metrics.prs_total [owner; name]) (float_of_int n_prs);
      if List.length refs < n_branches then
        Log.warn (fun f -> f "Too many branches in %s/%s (%d)" owner name n_branches);
      if List.length prs < n_prs then
        Log.warn (fun f -> f "Too many open PRs in %s/%s (%d)" owner name n_prs);
      let add xs map = List.fold_left (fun acc x -> Ref_map.add x.Commit_id.id (t, x) acc) map xs in
      Ref_map.empty
      |> add refs
      |> add prs
      |> fun all_refs -> { default_ref; all_refs }
    with ex ->
      let pp f j = Yojson.Safe.pretty_print f j in
      Log.err (fun f -> f "@[<v2>Invalid JSON: %a@,%a@]" Fmt.exn ex pp json);
      raise ex

let make_ci_refs_monitor t repo =
  let read () =
    Lwt.catch
      (fun () -> get_ci_refs t repo >|= Stdlib.Result.ok)
      (fun ex -> Lwt_result.fail @@ `Msg (Fmt.str "GitHub query for %a failed: %a" Repo_id.pp repo Fmt.exn ex))
  in
  let watch refresh =
    let owner_name = Printf.sprintf "%s/%s" repo.owner repo.name in
    let rec aux x =
      x >>= fun () ->
      let x = await_event ~owner_name in
      refresh ();
      Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
      aux x
    in
    let x = await_event ~owner_name in
    let thread =
      Lwt.catch
        (fun () -> aux x)
        (function
          | Lwt.Canceled -> Lwt.return_unit  (* (could clear metrics here) *)
          | ex -> Log.err (fun f -> f "ci_refs thread failed: %a" Fmt.exn ex); Lwt.return_unit
        )
    in
    Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit)
  in
  let pp f = Fmt.pf f "Watch %a CI refs" Repo_id.pp repo in
  Current.Monitor.create ~read ~watch ~pp

let refs t repo =
  Current.Monitor.get (
    match Repo_map.find_opt repo t.ci_refs_monitors with
    | Some i -> i
    | None ->
      let i = make_ci_refs_monitor t repo in
      t.ci_refs_monitors <- Repo_map.add repo i t.ci_refs_monitors;
      i
  )

let to_ci_refs refs =
  refs
  |> Ref_map.remove (`Ref "refs/heads/gh-pages")
  |> Ref_map.bindings
  |> List.map snd

let now () =
  Ptime.of_float_s (Unix.gettimeofday ()) |> function
  | Some t -> t
  | None -> Fmt.failwith "Failed to get current time"

let to_ptime str =
  Ptime.of_rfc3339 str |> function
  | Ok (t, _, _) -> t
  | Error (`RFC3339 (_, e)) -> Fmt.failwith "%a" Ptime.pp_rfc3339_error e
  
(** Check if the elapsed time from timestamps [start, finish] is within the [cutoff] duration *)
let active_date_cutoff ~start ~finish cutoff = 
  let diff = Ptime.diff finish start in
  match Ptime.Span.to_int_s diff with 
    | Some s -> (Duration.to_sec cutoff) - s > 0 
    | None -> Fmt.failwith "Failed to calculate ptime diff: %a" Ptime.Span.pp diff

let ci_refs ?(staleness=None) t repo =
  let+ refs =
    Current.component "%a CI refs" Repo_id.pp repo |>
    let> () = Current.return () in
    refs t repo
  in
  match staleness with 
    | None -> to_ci_refs refs.all_refs 
    | Some n ->
      let cutoff x s = 
        active_date_cutoff ~start:(to_ptime x.Commit_id.committed_date) ~finish:(now ()) s  
      in
      let is_default = function 
        | { Commit_id.id = `Ref t; _ } -> String.equal refs.default_ref t 
        | _ -> false 
      in 
      to_ci_refs (Ref_map.filter (fun _ (_, x) -> is_default x || cutoff x n) refs.all_refs)

let head_of t repo id =
  Current.component "%a@,%a" Repo_id.pp repo Ref.pp id |>
  let> () = Current.return () in
  refs t repo
  |> Current.Primitive.map_result @@ function
  | Error _ as e -> e
  | Ok refs ->
    match Ref_map.find_opt id refs.all_refs with
    | Some x -> Ok x
    | None -> Error (`Msg (Fmt.str "No such ref %a/%a" Repo_id.pp repo Ref.pp id))

module Commit = struct
  module Set_status = struct
    let id = "github-set-status"

    type nonrec t = t

    module Key = struct
      type t = {
        commit : Commit_id.t;
        context : string;
      }

      let to_json { commit; context } =
        `Assoc [
          "commit", `String (Commit_id.digest commit);
          "context", `String context
        ]

      let digest t = Yojson.Safe.to_string (to_json t)
    end

    module Value = Status

    module Outcome = Current.Unit

    let auto_cancel = true

    let pp f ({ Key.commit; context }, status) =
      Fmt.pf f "Set %a/%s to@ %a"
        Commit_id.pp commit
        context
        Value.pp status

    let publish t job key status =
      Current.Job.start job ~pool ~level:Current.Level.Above_average >>= fun () ->
      let {Key.commit; context} = key in
      let body = `Assoc (("context", `String context) :: Value.json_items status) in
      get_token t >>= function
      | Error (`Msg m) -> Lwt.fail_with m
      | Ok token ->
        let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ token) in
        let uri = status_endpoint
            ~owner_name:commit.Commit_id.owner_name
            ~commit:commit.Commit_id.hash
        in
        Current.Job.log job "@[<v2>POST %a:@,%a@]"
          Uri.pp uri
          (Yojson.Safe.pretty_print ~std:true) body;
        let body = body |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string in
        Lwt.try_bind
          (fun () ->
             Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
             Cohttp_lwt.Body.to_string body >|= fun body -> (resp, body)
          )
          (fun (resp, body) ->
             match Cohttp.Response.status resp with
             | `Created -> Lwt_result.return ()
             | err ->
               Log.warn (fun f -> f "@[<v2>%a failed: %s@,%s@]"
                            pp (key, status)
                            (Cohttp.Code.string_of_status err)
                            body);
               Lwt_result.fail (`Msg "Failed to set GitHub status")
          )
          (fun ex ->
               Log.warn (fun f -> f "@[<v2>%a failed: %a@]"
                            pp (key, status)
                            Fmt.exn ex);
               Lwt_result.fail (`Msg "Failed to set GitHub status")
          )
  end

  module Set_status_cache = Current_cache.Output(Set_status)

  type t = commit

  let uri (_, commit) = Commit_id.uri commit

  let id (_, commit_id) = Commit_id.to_git commit_id

  let compare (_, a) (_, b) = Commit_id.compare a b

  let owner_name (_, id) = id.Commit_id.owner_name

  let repo_id t =
    let full = owner_name t in
    match Astring.String.cut ~sep:"/" full with
    | Some (owner, name) -> { Repo_id.owner; name}
    | None -> Fmt.failwith "Invalid owner_name %S" full

  let hash (_, id) = id.Commit_id.hash

  let committed_date (_, id) = id.Commit_id.committed_date

  let pp = Fmt.using snd Commit_id.pp

  let set_status commit context status =
    Current.component "set_status" |>
    let> (t, commit) = commit
    and> status = status in
    Set_status_cache.set t {Set_status.Key.commit; context} status
end

module Repo = struct
  type nonrec t = t * Repo_id.t

  let id = snd
  let pp = Fmt.using id Repo_id.pp
  let compare a b = Repo_id.compare (id a) (id b)

  let head_commit t =
    Current.component "head" |>
    let> (api, repo) = t in
    Current.Monitor.get (
      match Repo_map.find_opt repo api.head_monitors with
      | Some i -> i
      | None ->
        let i = make_head_commit_monitor api repo in
        api.head_monitors <- Repo_map.add repo i api.head_monitors;
        i
    )

  let ci_refs t =
    let+ refs =
      Current.component "CI refs" |>
      let> (api, repo) = t in
      refs api repo
    in
    to_ci_refs refs.all_refs
end

module Anonymous = struct
  let ref_endpoint ~owner ~name gref =
    let gref = Ref.to_git gref in
    match Astring.String.cut ~sep:"/" gref with
    | Some ("refs", gref) -> Uri.of_string (Printf.sprintf "https://api.github.com/repos/%s/%s/git/ref/%s" owner name gref)
    | Some _ -> Fmt.failwith "Ref %S does not start with 'refs/'!" gref
    | None -> Fmt.failwith "Missing '/' in ref %S" gref

  let query_head { Repo_id.owner; name } gref =
    let uri = ref_endpoint ~owner ~name gref in
    Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    match Cohttp.Response.status resp with
    | `OK | `Created ->
      let json = Yojson.Safe.from_string body in
      Log.debug (fun f -> f "@[<v2>Got response:@,%a@]" Yojson.Safe.pp json);
      let open Yojson.Safe.Util in
      json |> member "object" |> member "sha" |> to_string
    | err -> Fmt.failwith "@[<v2>Error accessing GitHub App API at %a: %s@,%s@]"
               Uri.pp uri
               (Cohttp.Code.string_of_status err)
               body

  let head_of (repo : Repo_id.t) gref =
    let owner_name = Printf.sprintf "%s/%s" repo.owner repo.name in
    let read () =
      Lwt.try_bind
        (fun () -> query_head repo gref)
        (fun hash ->
           let id = { Commit_id.owner_name; hash; id = gref; committed_date = "" } in
           Lwt_result.return (Commit_id.to_git id)
        )
        (fun ex ->
           Log.warn (fun f -> f "GitHub query_head failed: %a" Fmt.exn ex);
           Lwt_result.fail (`Msg (Fmt.str "Failed to get head of %a:%a" Repo_id.pp repo Ref.pp gref))
        )
    in
    let watch refresh =
      let rec aux x =
        x >>= fun () ->
        let x = await_event ~owner_name in
        refresh ();
        Lwt_unix.sleep 10.0 >>= fun () ->   (* Limit updates to 1 per 10 seconds *)
        aux x
      in
      let x = await_event ~owner_name in
      let thread =
        Lwt.catch
          (fun () -> aux x)
          (function
            | Lwt.Canceled -> Lwt.return_unit
            | ex -> Log.err (fun f -> f "Anonymous.head thread failed: %a" Fmt.exn ex); Lwt.return_unit
          )
      in
      Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit)
    in
    let pp f = Fmt.pf f "Query head of %a:%a" Repo_id.pp repo Ref.pp gref in
    let monitor = Current.Monitor.create ~read ~watch ~pp in
    Current.component "%a:%a" Repo_id.pp repo Ref.pp gref |>
    let> () = Current.return () in
    Current.Monitor.get monitor
end

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
