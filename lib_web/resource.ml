open Lwt.Infix

class virtual t = object (self)
  method private get ctx =
    Context.respond_error ctx `Bad_request "Bad method"

  method private post ctx (_ : string) =
    Context.respond_error ctx `Bad_request "Bad method"

  method get_raw site request =
    Context.of_request ~site request >>= fun ctx ->
    self#get ctx

  method post_raw site request body =
    Context.of_request ~site request >>= fun ctx ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let data = Uri.query_of_encoded body in
    match List.assoc_opt "csrf" data |> Option.value ~default:[] with
    | [got] when got = Utils.csrf_token ->
      self#post ctx body
    | _ -> Context.respond_error ctx `Bad_request "Bad CSRF token"
end
