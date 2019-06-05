open Current.Syntax

type source = Fpath.t

let revdeps src =
  "get-revdeps" |>
  let** _src = src in
  Current.return [
    Current_git.commit ~repo:"example.org/foo" ~hash:"111";
    Current_git.commit ~repo:"example.org/bar" ~hash:"222";
  ]
