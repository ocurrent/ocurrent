open Current.Syntax

module Git = Current_git_test

type source = Fpath.t

let revdeps src =
  "get-revdeps" |>
  let** _src = src in
  Current.return [
    Git.commit ~repo:"example.org/foo" ~hash:"111";
    Git.commit ~repo:"example.org/bar" ~hash:"222";
  ]
