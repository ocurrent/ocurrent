open Current.Syntax

module RepoMap = Map.Make(String)

let state = ref RepoMap.empty

type commit = {
  repo : string;
  hash : string;
}

let commit ~repo ~hash = { repo; hash }

let complete_clone {repo; hash} =
  let r = Fpath.v ("src-" ^ hash) in
  state := RepoMap.add repo (`Cloned r) !state

let reset () =
  state := RepoMap.empty

let fetch c = "fetch" |>
  let** { repo; hash = _ } = c in
  let s =
    match RepoMap.find_opt repo !state with
    | None ->
      let s = `Cloning in
      state := RepoMap.add repo s !state;
      s
    | Some s -> s
  in
  match s with
  | `Cloning ->
    Current.track (Fmt.strf "git clone %S" repo) Current.pending
  | `Cloned path ->
    Current.return path
