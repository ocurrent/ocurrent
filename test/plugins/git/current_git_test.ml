open Current.Syntax

let src = Logs.Src.create "test.git" ~doc:"OCurrent test git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module RepoMap = Map.Make(String)

type clone_state = Fpath.t Current.Input.t * Fpath.t Lwt.u

let state : clone_state RepoMap.t ref = ref RepoMap.empty

let clone repo =
  let ready, set_ready = Lwt.wait () in
  let watch =
    object
      method pp f = Fmt.pf f "git clone %S" repo
      method changed = Lwt.map ignore ready
      method release = ()
    end
  in
  let get () =
    match Lwt.state ready with
    | Lwt.Sleep -> Error `Pending, [watch]
    | Lwt.Fail f -> Error (`Msg (Printexc.to_string f)), []
    | Lwt.Return x -> Ok x, []
  in
  Current.Input.of_fn get, set_ready

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
  | Some (_, set) ->
    let r = Fpath.v ("src-" ^ hash) in
    Lwt.wakeup set r

let reset () =
  state := RepoMap.empty

let fetch c = "fetch" |>
  let** { Commit.repo; hash = _ } = c in
  match RepoMap.find_opt repo !state with
  | None ->
    let input, set = clone repo in
    let s = (input, set) in
    state := RepoMap.add repo s !state;
    Current.track input
  | Some (i, _) ->
    Current.track i
