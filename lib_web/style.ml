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
    overflow: hidden;
  }

  nav ul {
    float: left;
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

  nav ul.right {
    float: right;
  }

  nav ul.right form {
    padding: 0;
    background: none!important;
  }

  nav ul.right button {
    background: black;
    border: none;
    color: #ddd;
    text-decoration: none;
    padding-left: 0.5em;
    padding-right: 0.5em;
  }

  nav ul.right button:hover {
    color: black;
    background: yellow;
  }

  nav a {
    color: #ddd;
    text-decoration: none;
    padding-left: 0.5em;
    padding-right: 0.5em;
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

  table.log-rules {
    width: 100%;
  }

  table td input {
    width: 100%;
    box-sizing: border-box;
  }

  table.log-rules td.score {
    width: 4em;
  }

  table pre {
    margin: 0;
  }

  div.build-history ol {
    display: inline;
    padding: 0;
  }

  div.build-history ol li {
    display: inline
  }

  div.build-history ol li + li:before {
      content: ", ";
      padding: 0 .2em;
  }

  ul.query-form {
    list-style-type: none;
    padding-left: 0;
    width: 100%;
  }

  ul.query-form li {
    display: inline
  }

  ul.query-form li select {
    margin-left: 0.5em;
    margin-right: 2em;
  }

  ul.query-form li input {
    margin-left: 0.5em;
    margin-right: 2em;
  }
|} ^ Current_ansi.css

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get _ctx =
    let headers = Cohttp.Header.init_with "Content-Type" "text/css" in
    Utils.Server.respond_string ~status:`OK ~headers ~body:css ()
    (*   Server.respond_file ~fname:"style.css" () *)
end
