module Server = Cohttp_lwt_unix.Server

(* Very primitive CSRF protection. *)
let csrf_token =
  Random.self_init ();
  Random.int64 Int64.max_int |> Int64.to_string

let string_of_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.strf "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let respond_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  Server.respond_error ~status ~headers ~body ()
