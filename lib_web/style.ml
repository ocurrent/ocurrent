let css =
  {|
  body {
    font-family: Roboto, sans-serif;
    background: white;
    color: #002000;
    margin: 0;
    padding: 0;
  }

  h2 {
    font-weight: 500;
  }

  a {
    color: #0F0EC0;
    text-decoration: none;
  }

  a:visited {
    color: #0F0EC0;
  }

  a:hover, a:active {
    color: #0200e9;
    text-decoration: underline;
  }

  code {
    font-family: 'Roboto Mono', monospace;
    font-size: 0.97rem;
  }

  input[type=text] {
    padding: 6px 12px;
  }

  input[type=submit] {
    padding: 6px 12px;
    cursor: pointer;
  }

  select {
    padding: 6px 0 6px 4px;
  }

  @media only screen and (min-width: 1000px) {
    select {
      min-width: 7em;
    }
  }

  nav {
    display: flex;
    justify-content: flex-start;
    align-items: center;
    margin: 0;
    padding: 0;
    background: #202020;
    color: #ddd;
    overflow: hidden;
    user-select: none;
  }

  nav img {
     margin: 8px;
     object-fit: contain;
     height: 29px;
  }

  nav .site-name {
    font-family: 'Roboto Mono', monospace;
    font-size: 0.97rem;
    margin: 0 1rem 0 0;
  }

  nav ul {
    display: flex;
    justify-content: flex-start;
    align-items: stretch;
    list-style-type: none;
    margin: 0;
    padding: 0;
  }

  nav li {
    display: flex;
    justify-content: flex-start;
    align-items: stretch;
  }

  nav ul.right {
    margin-left: auto;
    margin-right: 1rem;
  }

  nav ul.right form {
    padding: 0;
    background: none!important;
  }

  nav ul.right button {
    background: #202020;
    border: none;
    color: #ddd;
    text-decoration: none;
    padding-left: 0.5em;
    padding-right: 0.5em;
  }

  nav ul.right button:hover {
    color: white;
    background: linear-gradient(180deg, rgba(0, 0, 0, 0) 0%, rgba(0, 0, 0, 0) 50%, rgba(236,125,11,0.9) 51%, rgba(229,83,14,0.9) 100%);
  }

  nav a.nav-menu-option {
    display: block;
    color: #ddd;
    text-decoration: none;
    padding: 2ex 0.75em;
    transition: background 150ms, color 300ms;
    background: linear-gradient(180deg, rgba(0, 0, 0, 0) 0%, rgba(0, 0, 0, 0) 50%, rgba(236,125,11,0.9) 51%, rgba(229,83,14,0.9) 100%);
    background-size: 1px calc(200% + 1px);
  }

  nav a.nav-menu-option:visited {
    color: #ddd;
  }

  nav a.nav-menu-option:hover, nav a.nav-menu-option:active {
    color: white;
    background-position-y: -8px;
  }

  div#main {
    margin: 0.5em;
  }

  div#main object {
    max-width: 100%;
  }

  table.table {
    background: #fafafa;
    box-shadow: 0 1px 3px rgb(0 0 0 / 12%), 0 1px 2px rgb(0 0 0 / 24%);
    padding: 0;
    margin: 1ex 0 3ex 0;
    border-collapse: collapse;
    border: 2px solid transparent;
    border-radius: 8px;
    overflow: hidden;
  }

  table.table thead {
    background: rgb(229,103,14);
    background: linear-gradient(172deg, rgba(236,125,11,0.9) 0%, rgba(229,83,14,0.9) 100%);
    color: white;
  }

  table.table th {
    font-weight: 400;
    text-align: left;
    padding: 1ex 0.5em;
  }

  @media only screen and (min-width: 1000px) {
    table.table th {
      padding: 1.5ex 1em;
    }
  }

  table.table tbody tr {
    border-bottom: white 2px solid;
    transition: background 300ms;
  }

  table.table tbody tr:hover {
    background: #f0f0f0;
  }

  table.table td {
    padding: 1ex 0.5em;
  }

  @media only screen and (min-width: 1000px) {
    table.table td {
      padding: 1ex 1em;
    }
  }

  table.table tr td {
    padding-left: 0.5em;
    padding-right: 0.5em;
    border: 1px solid black;
  }

  table.table tr:nth-child(even) {
    background-color: gainsboro;
  }

  form {
    padding: 0.2em;
  }

  table.log-rules {
    width: 100%;
    max-width: 1200px;
  }

  table td input {
    box-sizing: border-box;
  }

  table.log-rules td input {
    width: 100%;
  }

  table.log-rules td.score {
    width: 4em;
  }

  table pre {
    margin: 0;
  }

  form.log-rules input {
    margin-right: 1rem;
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
    margin: 0.5ex 1em;
    padding-left: 0;
    width: 100%;
    display: flex;
    justify-content: flex-start;
    align-items: center;
    flex-wrap: wrap;
  }

  @media only screen and (max-width: 500px) {
    ul.query-form {
      flex-direction: column;
      align-items: flex-start;
    }
  }

  ul.query-form li {
    margin: 0.5ex 0;
    display: flex;
    justify-content: flex-start;
    align-items: center;
  }

  ul.query-form li select {
    margin-left: 0.5em;
    margin-right: 2em;
  }

  ul.query-form li input {
    margin-left: 0.5em;
    margin-right: 2em;
  }

  .settings-form select {
    margin-right: 1em;
  }

  #line-numbers {
    color:black;
    display:block;
  }

  #line-numbers {
    float:left;
    margin:0 1em 0 -1em;
    border-right:1px solid;
    text-align:right;
  }

  #line-numbers span {
    display:block;
    padding:0 .5em 0 1em;
  }

  #line-numbers .highlight {
    background: lightgrey;
  }

|}
  ^ Ansi.css

let r =
  object
    inherit Resource.t

    val! can_get = `Viewer

    method! private get _ctx =
      let headers = Cohttp.Header.init_with "Content-Type" "text/css" in
      Utils.Server.respond_string ~status:`OK ~headers ~body:css ()
    (*   Server.respond_file ~fname:"style.css" () *)
  end
