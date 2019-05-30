open Current.Syntax

type source = Fpath.t
type image = string

let build_on platform ~src =
  "build-" ^ platform |>
  let** c = src in
  Current.return @@ Fmt.strf "%s-image-%s" platform (Fpath.to_string c)

let build c =
  "build" |>
  let** c = c in
  Current.return @@ "image-" ^ Fpath.to_string c

let build ?on src =
  match on with
  | None -> build src
  | Some platform -> build_on platform ~src

let run image ~cmd =
  Fmt.strf "docker run %a" Fmt.(list string) cmd |>
  let** _ = image in
  Current.return ()

let push image ~tag =
  Fmt.strf "docker push %s" tag |>
  let** _ = image in
  Current.return ()
