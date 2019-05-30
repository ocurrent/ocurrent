open Current.Syntax

type source = Fpath.t

let revdeps src =
  "get-revdeps" |>
  let** src = src in
  Fpath.to_string src |> String.to_seq |> List.of_seq
  |> List.map (fun c -> String.make 1 c |> Current_git.commit_of_string)
  |> Current.return
