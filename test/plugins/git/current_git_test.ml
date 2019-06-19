open Current.Syntax

let src = Logs.Src.create "test.git" ~doc:"OCurrent test git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module RepoMap = Map.Make(String)

let state : (Fpath.t, [`Msg of string]) result Lwt.u RepoMap.t ref = ref RepoMap.empty

module Commit = struct
  type t = {
    repo : string;
    hash : string;
  }

  let v ~repo ~hash = { repo; hash }

  let pp f {repo; hash} =
    Fmt.pf f "%s#%s" repo hash

  let equal = (=)
  let compare = compare
end

let complete_clone {Commit.repo; hash} =
  match RepoMap.find_opt repo !state with
  | None -> Fmt.failwith "No clone started for %S!" repo
  | Some set ->
    let r = Fpath.v ("src-" ^ hash) in
    Lwt.wakeup set (Ok r)

module Builder = struct
  type t = No_context
  module Key = Commit
  module Value = Fpath

  let pp f key = Fmt.pf f "git clone %S" key.Commit.repo

  let build ~switch:_ No_context (key : Key.t) =
    let ready, set_ready = Lwt.wait () in
    state := RepoMap.add key.Commit.repo set_ready !state;
    ready

  let auto_cancel = false

  let level _ _ = Current.Level.Average
end

module C = Current_cache.Make(Builder)

let fetch c =
  "fetch" |>
  let** c = c in
  C.get Builder.No_context c

let reset () =
  state := RepoMap.empty;
  C.reset ()
