open Current.Syntax

type commit = string

let commit_of_string x = x

let fetch c = "fetch" |>
  let** c = c in
  Current.return @@ Fpath.v ("src-" ^ c)
