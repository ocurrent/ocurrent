open Current.Syntax
open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let repositories_total =
    let help = "Total number of active repositories" in
    Gauge.v_label ~label_name:"account" ~help ~namespace ~subsystem "repositories_total"
end

type t = {
  iid : int;
  account : string;
  api : Api.t;
  repos : Api.Repo.t list Current.Monitor.t;
}

let installation_repositories_cond = Lwt_condition.create ()

let input_installation_repositories_webhook () = Lwt_condition.broadcast installation_repositories_cond ()

let pp f t = Fmt.string f t.account

let compare a b = compare a.iid b.iid

let list_repositories_endpoint = Uri.of_string "https://api.github.com/installation/repositories"

let list_repositories ~api ~token ~account =
  let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ token) in
  let headers = Cohttp.Header.add headers "accept" "application/vnd.github.machine-man-preview+json" in
  let uri = list_repositories_endpoint  in
  Log.debug (fun f -> f "Get repositories for %S from %a" account Uri.pp uri);
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Response.status resp with
  | `OK ->
    let json = Yojson.Safe.from_string body in
    Log.debug (fun f -> f "@[<v2>Got response:@,%a@]" Yojson.Safe.pp json);
    let open Yojson.Safe.Util in
    let repos = json |> member "repositories" |> to_list in
    Prometheus.Gauge.set (Metrics.repositories_total account) (float_of_int (List.length repos));
    repos |> List.map @@ fun r ->
    let name = r |> member "name" |> to_string in
    api, Repo_id.{ owner = account; name }
  | err -> Fmt.failwith "@[<v2>Error accessing GitHub installation API at %a: %s@,%s@]"
             Uri.pp uri
             (Cohttp.Code.string_of_status err)
             body

let v ~iid ~account ~api =
  let read () =
    Api.get_token api >>= function
    | Error (`Msg m) -> Lwt.fail_with m
    | Ok token ->
      list_repositories ~api ~token ~account >|= Stdlib.Result.ok
  in
  let watch refresh =
    let rec aux event =
      event >>= fun () ->
      let event = Lwt_condition.wait installation_repositories_cond in
      refresh ();
      aux event
    in
    let thread = aux (Lwt_condition.wait installation_repositories_cond) in
    Lwt.return (fun () -> Lwt.cancel thread; Lwt.return_unit) in
  let pp f = Fmt.string f account in
  let repos = Current.Monitor.create ~read ~watch ~pp in
  { iid; account; api; repos }

let api t = t.api

let repositories t =
  Current.component "list repos" |>
  let> t = t in
  Current.Monitor.input t.repos
