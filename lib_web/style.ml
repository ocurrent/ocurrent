module Server = Cohttp_lwt_unix.Server

let css = {|
  body {
    margin: 0;
    padding: 0;
  }

  nav {
    margin: 0;
    padding: 0.1em;
    display: block;
    background: black;
    color: #ddd;
  }

  nav ul {
    list-style-type: none;
    margin: 0;
    padding: 0;
  }

  nav li {
    display: inline;
  }

  nav li:hover {
    background: yellow;
  }

  nav a {
    color: #ddd;
    text-decoration: none;
    padding-right: 1em;
  }

  nav a:hover {
    color: black;
  }

  div#main {
    margin: 0.5em;
  }

  div#main object {
    max-width: 100%;
  }

  table.table {
    border: 1px solid black;
    padding: 0;
  }

  table.table th {
    background: #888;
    color: white;
    padding: 0.1em;
  }

  form {
    padding: 0.2em;
  }
|}

let get () =
  let headers = Cohttp.Header.init_with "Content-Type" "text/css" in
  Server.respond_string ~status:`OK ~headers ~body:css ()
(*   Server.respond_file ~fname:"style.css" () *)
