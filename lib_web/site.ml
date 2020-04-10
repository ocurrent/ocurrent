module Sess = struct
  module Backend = Session.Lift.IO(Lwt)(Sqlite_session)
  include Session_cohttp_lwt.Make(Backend)
end

type t = {
  name : string;
  authn : (csrf:string -> Uri.t) option;
  has_role : User.t option -> Role.t -> bool;
  secure_cookies : bool;
  session_backend : Sess.backend;
}

let allow_all _ _ = true

let v ?(name="OCurrent") ?authn ?(secure_cookies=false) ~has_role () =
  let db = Lazy.force Current.Db.v in
  { name; authn; has_role; secure_cookies; session_backend = Sqlite_session.create db }
