open Current.Syntax

module RepoMap = Map.Make(String)

class clone repo =
  let ready, set_ready = Lwt.wait () in
  object
    method pp f = Fmt.pf f "git clone %S" repo
    method changed = ready
    method set_complete = Lwt.wakeup set_ready ()
  end

type clone_state =
  | Cloning of clone
  | Cloned of Fpath.t

let state : clone_state RepoMap.t ref = ref RepoMap.empty

module Commit = struct
  type t = {
    repo : string;
    hash : string;
  }

  let v ~repo ~hash = { repo; hash }

  let pp f {repo; hash} =
    Fmt.pf f "%s#%s" repo hash

  let equal = (=)
end

let complete_clone {Commit.repo; hash} =
  match RepoMap.find_opt repo !state with
  | None -> Fmt.failwith "No clone started for %S!" repo
  | Some Cloned _ -> Fmt.failwith "Clone already complete for %S!" repo
  | Some Cloning c ->
    let r = Fpath.v ("src-" ^ hash) in
    state := RepoMap.add repo (Cloned r) !state;
    c#set_complete

let reset () =
  state := RepoMap.empty

let fetch c = "fetch" |>
  let** { Commit.repo; hash = _ } = c in
  let s =
    match RepoMap.find_opt repo !state with
    | None ->
      let s = Cloning (new clone repo) in
      state := RepoMap.add repo s !state;
      s
    | Some s -> s
  in
  match s with
  | Cloning i ->
    Current.track (i :> Current.Input.t) (Current.pending ())
  | Cloned path ->
    Current.return path
