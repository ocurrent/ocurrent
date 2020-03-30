open Current.Syntax

module Git = Current_git_test

type source = Fpath.t

let revdeps src =
  Current.component "get-revdeps" |>
  let> src = src in
  match Fpath.to_string src with
  | "src-123" ->
    Current.Primitive.const [
      Git.Commit.v ~repo:"example.org/foo" ~hash:"111";
      Git.Commit.v ~repo:"example.org/bar" ~hash:"222";
    ]
  | _ ->
    Current.Primitive.const []
