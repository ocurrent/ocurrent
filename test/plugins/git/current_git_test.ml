open Current.Syntax

module RepoMap = Map.Make(String)

let clone repo =
  let ready, set_ready = Lwt.wait () in
  let get () =
    let v =
      match Lwt.state ready with
      | Lwt.Sleep -> Error `Pending
      | Lwt.Fail f -> Error (`Msg (Printexc.to_string f))
      | Lwt.Return x -> Ok x
    in
    let watch =
      object
        method pp f = Fmt.pf f "git clone %S" repo
        method changed = Lwt.map ignore ready
      end
    in
    v, watch
  in
  Current.Input.of_fn get, set_ready

type clone_state =
  | Cloning of Fpath.t Current.Input.t * Fpath.t Lwt.u
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
  | Some Cloning (_, set) ->
    let r = Fpath.v ("src-" ^ hash) in
    state := RepoMap.add repo (Cloned r) !state;
    Lwt.wakeup set r

let reset () =
  state := RepoMap.empty

let fetch c = "fetch" |>
  let** { Commit.repo; hash = _ } = c in
  let s =
    match RepoMap.find_opt repo !state with
    | None ->
      let input, set = clone repo in
      let s = Cloning (input, set) in
      state := RepoMap.add repo s !state;
      s
    | Some s -> s
  in
  match s with
  | Cloning (i, _) -> Current.track i
  | Cloned path -> Current.return path
