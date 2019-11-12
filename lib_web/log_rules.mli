val render : unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
val handle_post : (string * string list) list -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
