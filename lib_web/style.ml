module Server = Cohttp_lwt_unix.Server

let css = {|
  body {
    margin: 0;
    padding: 0;
  }

  h1 {
    margin: 0;
    padding: 0.1em;
    display: block;
    background: black;
    color: #ddd;
    font-size: 100%;
  }

  h1 a {
    color: #ddd;
    text-decoration: none;
  }

  div#main {
    margin: 0.5em;
  }
|}

let get () =
  let headers = Cohttp.Header.init_with "Content-Type" "text/css" in
  Server.respond_string ~status:`OK ~headers ~body:css ()
(*   Server.respond_file ~fname:"style.css" () *)
