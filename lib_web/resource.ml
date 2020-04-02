open Lwt.Infix

class virtual t = object (self)
  method private get (_ : Cohttp.Request.t) = Utils.respond_error `Bad_request "Bad method"
  method private post (_ : Cohttp.Request.t) (_ : string) = Utils.respond_error `Bad_request "Bad method"

  method get_raw = self#get

  method post_raw request body =
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let data = Uri.query_of_encoded body in
    match List.assoc_opt "csrf" data |> Option.value ~default:[] with
    | [got] when got = Utils.csrf_token -> self#post request body
    | _ -> Utils.respond_error `Bad_request "Bad CSRF token"
end
