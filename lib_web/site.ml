module Sess = struct
  module Backend = Session.Lift.IO(Lwt)(Sqlite_session)
  include Session_cohttp_lwt.Make(Backend)
end

class type ['site] raw = object
  method get_raw : 'site -> Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  method post_raw : 'site -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  method nav_link : string option
end

type t = {
  name : string;
  authn : (csrf:string -> Uri.t) option;
  has_role : User.t option -> Role.t -> bool;
  secure_cookies : bool;
  http_only: bool;
  session_backend : Sess.backend;
  router : t raw Routes.router;
  nav_links : (string * string) list;   (* Label, path *)
  refresh_pipeline : int option;
}

class type raw_resource = [t] raw

let allow_all _ _ = true

let v ?(name="OCurrent") ?authn ?(secure_cookies=false) ?(http_only=false) ?refresh_pipeline ~has_role routes =
  let db = Lazy.force Current.Db.v in
  let router = Routes.one_of routes in
  let nav_links = routes |> List.filter_map (fun route ->
      let target = Fmt.to_to_string Routes.pp_route route in
      if String.contains target ':' then None else (
        let resource =
          match Routes.match' router ~target with
          | Routes.FullMatch v -> v
          | MatchWithTrailingSlash v -> v
          | NoMatch -> failwith "No match found"
        in
        Option.map (fun label -> (label, target)) resource#nav_link
      )
    ) in
  { name; authn; has_role; secure_cookies; http_only; session_backend = Sqlite_session.create db;
    router; nav_links; refresh_pipeline }
