open Current.Syntax

module Git = Current_git_test

type source = Fpath.t

let revdeps src =
  Current.component "get-revdeps" |>
  let** _src = src in
  Current.return [
    Git.Commit.v ~repo:"example.org/foo" ~hash:"111";
    Git.Commit.v ~repo:"example.org/bar" ~hash:"222";
  ]
