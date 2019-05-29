open OCurrent.Syntax

type commit = string
type source = string
type binary = string

let fetch c = "fetch" |>
  let** c = c in
  OCurrent.return @@ "src-" ^ c

let build_on platform ~src = "build-" ^ platform |>
  let** c = src in
  OCurrent.return @@ Fmt.strf "%s-binary-%s" platform c

let build c = "build" |>
  let** c = c in
  OCurrent.return @@ "binary-" ^ c

let test1 bin = "test" |>
  let** _ = bin in
  OCurrent.fail "Broken"

let test2 src bin = "test" |>
  let** _ = src
  and* _ = bin in
  OCurrent.return ()

let deploy x = "deploy" |>
  let** _ = x in
  OCurrent.return ()

let build ?on src =
  match on with
  | None -> build src
  | Some platform -> build_on platform ~src

let test ?src bin =
  match src with
  | None -> test1 bin
  | Some src -> test2 src bin

let revdeps src =
  "get-revdeps" |>
  let** src = src in
  String.to_seq src |> List.of_seq |> List.map (String.make 1) |> OCurrent.return

let test_commit = "123"
