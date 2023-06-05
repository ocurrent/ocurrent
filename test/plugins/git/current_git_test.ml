open Current.Syntax

let src = Logs.Src.create "test.git" ~doc:"OCurrent test git plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module RepoMap = Map.Make(String)

let state : (Fpath.t, [`Msg of string]) result Eio.Promise.u RepoMap.t ref = ref RepoMap.empty

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
  let digest { repo; hash } = Fmt.str "%s#%s" repo hash
end

let complete_clone {Commit.repo; hash} =
  match RepoMap.find_opt repo !state with
  | None -> Fmt.failwith "No clone started for %S!" repo
  | Some set ->
    let r = Fpath.v ("src-" ^ hash) in
    Eio.Promise.resolve set (Ok r)

module Clone = struct
  type t = No_context
  module Key = Commit
  module Value = struct
    type t = Fpath.t
    let marshal = Fpath.to_string
    let unmarshal = Fpath.v
  end

  let id = "git-clone"

  let pp f key = Fmt.pf f "git clone %S" key.Commit.repo

  let build No_context job (key : Key.t) =
    Current.Job.start job ~level:Current.Level.Average;
    let ready, set_ready = Eio.Promise.create () in
    Current.Job.on_cancel job (fun _ ->
        if not (Eio.Promise.is_resolved ready) then begin
          Eio.Promise.resolve_error set_ready (`Msg "Cancelled")
        end
      );
    state := RepoMap.add key.Commit.repo set_ready !state;
    Eio.Promise.await ready

  let auto_cancel = true
end

module C = Current_cache.Make(Clone)

let fetch ~sw c =
  Current.component "fetch" |>
  let> c = c in
  C.get ~sw Clone.No_context c

let reset () =
  state := RepoMap.empty;
  C.reset ~db:true
