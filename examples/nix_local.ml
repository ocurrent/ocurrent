let program_name = "nix_local"

module Git = Current_git
module Github = Current_github

let () = Logging.init ()

open Lwt.Infix
let ( >>!= ) = Lwt_result.bind
open Current.Syntax
module S = Current_cache.S

module Drv = struct
  type t = {
    drv_path : string;
  }

  let cmd { drv_path } =
    [ "nix-store"; "--realise"; drv_path ]

  (* let pp_cmd = Fmt.(list ~sep:sp (quote string)) *)
  let pp f { drv_path } = Fmt.string f drv_path

  let pp_short f { drv_path } = Fmt.string f
    (String.index_opt drv_path '-' |> Option.fold ~none:drv_path ~some:(fun i ->
      String.sub drv_path (i+1) (String.length drv_path - (i + 1))
    ))

  let digest { drv_path } =
    Yojson.Safe.to_string @@ `Assoc [
      "drv", `String drv_path;
    ]
    
  let marshal { drv_path } = drv_path

  let unmarshal drv_path = { drv_path }
  
  let equal { drv_path = drva } { drv_path = drvb } = String.equal drva drvb

  let compare { drv_path = drva } { drv_path = drvb } = String.compare drva drvb
end

module Drv_list = struct
  type t = Drv.t list

  let pp = Fmt.list Drv.pp

  let digest drvs = String.concat "\n" (List.map Drv.digest drvs)

  (* TODO this could be cleaner as JSON *)
  let marshal drvs = String.concat "\n" (List.map Drv.marshal drvs)

  let unmarshal str = String.split_on_char '\n' str |> List.map Drv.unmarshal
end

module Realise = struct
  let id = "nix-store-realise"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = Drv

  module Value = Current.Unit

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    Current.Process.exec ~cancellable:true ~job ("", Key.cmd key |> Array.of_list)

  let pp = Drv.pp

  let auto_cancel = true
end

module Build_plan = struct
  (*
  We use a combination of:

  nix-store --realise --dry-run: get the set of possible inputs that need building
  nix-store --query --references: get the set of direct dependencies

  Many dependencies don't need to be built, and --dry-run prints transitive deps.
  So we take the intersection to get "direct deps that actually need building"
  *)
  let id = "nix-plan"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = Drv

  module Value = Drv_list
  
  module Dry_run = struct
    type section = Unknown | Build | Download
    type state = {
      section: section;
      build: Key.t list;
    }
    let initial = { section = Unknown; build = []; }
    let accumulate_output ~job line ({ section; build } as state) =
      let is_indented = try
        let first = String.get line 0 in
        Current.Job.log job "first char: %C" first;
        first = ' ' || first = '\t'
      with Invalid_argument _ -> false in

      let line = line |> String.trim in
      if is_indented then (
        (* Add to existing state *)
        match section with
          | Build -> (
            Current.Job.log job "Build: %s" line;
            let drv = Key.{ drv_path = line } in
            { build = drv :: build; section }
          )
          | Download -> (
            Current.Job.log job "Download: %s" line;
            state
          )
          | Unknown -> (
            Current.Job.log job "UNKNOWN ACTION: %s" line;
            state
          )
      ) else (
        if String.equal line "these derivations will be built:" then (
          { build; section = Build }
        ) else if String.equal line "these derivations will be downloaded:" then (
          { build; section = Download }
        ) else (
          Current.Job.log job "WARN: unknown section marker in --dry-run output: %s" line;
          { build; section = Unknown }
        )
      )

    let copy_to_log ~job src =
      let rec aux () =
        Lwt_io.read ~count:4096 src >>= function
        | "" -> Lwt.return_unit
        | data -> Current.Job.write job data; aux ()
      in
      aux ()
  end

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = [ "nix-store"; "--realise"; "--dry-run"; key.Key.drv_path ] in
    (* TODO could run in parallel *)
    Current.Process.exec_with ~cancellable:true ~job ("", cmd |> Array.of_list) (fun proc ->
      let open Dry_run in
      Current.Job.log job "waiting for exec...";
      (* can't use check_output since dry-run outputs to stderr *)
      let copy_thread = copy_to_log ~job proc#stdout in
      let stderr_lines = Lwt_io.read_lines proc#stderr in
      let stderr_lines = Lwt_stream.map (fun line ->
        Current.Job.log job "[stderr] %s" line;
        line
      ) stderr_lines in
      let result = Lwt_stream.fold (accumulate_output ~job) stderr_lines initial in
      copy_thread >>= fun () ->
      result |> Lwt.map (Result.ok)
    ) |> Lwt_result.map (fun state ->
      if state = initial then (
        Current.Job.log job "NOTE: no derivations need building"
      );
      Current.Job.log job "got state with build = %a" (Fmt.list Key.pp) state.build;
      let deps = state.build |> List.filter (fun drv ->
        not (Key.equal drv key)
      ) in
      Value.{ self = key; deps }
    )

  let pp = Key.pp

  let auto_cancel = true
end

module Eval = struct
  let id = "nix-eval"

  type t = {
    pool : unit Current.Pool.t option;
  }

  (* TODO this should track nixpkgs & opam2nix hashes *)
  module Key = Current.String
  module Value = Realise.Key

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = [ "nix-instantiate"; "--expr"; key ] |> Array.of_list in
    Current.Process.check_output ~cancellable:true ~job ("", cmd) >>!= fun drv ->
    let drv = String.trim drv in
    Current.Job.log job "Resolved -> %S" drv;
    Lwt_result.return { Realise.Key.drv_path = drv }

  let pp = Key.pp


  let auto_cancel = true
end

module Shell = struct
  let id = "nix-shell"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = struct
    type t = {
      expr : string;
      path : Fpath.t;
    }

    let digest { expr; path } =
      Yojson.Safe.to_string @@ `Assoc [
        "expr", `String expr;
        "path", `String (Fpath.to_string path);
      ]

      let pp f { expr; _ } = Fmt.string f expr (* TODO print both *)
  end

  module Value = Current.String

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = ["nix-shell"; "--expr"; key.Key.expr] in
    Current.Process.with_tmpdir ~prefix:"opam2nix-resolve-" (fun cwd ->
      (* TODO result instead of exceptions? *)
      Current.Process.exec ~cwd ~cancellable:true ~job ("", cmd |> Array.of_list) >>!= (fun () ->
        let path = Fpath.append cwd key.Key.path in
        Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string path) (fun file ->
          Lwt_io.read_lines file
            |> Lwt_stream.to_list
            |> Lwt.map (String.concat "\n")
        ) |> Lwt.map (Result.ok)
      )
    )

  let pp = Key.pp

  let auto_cancel = true
end

module Raw = struct
  module RealiseC = Current_cache.Make(Realise)

  let realise_one ?pool drv =
    RealiseC.get { Realise.pool } drv
    
  module Build_planC = Current_cache.Make(Build_plan)
  
  let build_plan ?pool drv =
    Build_planC.get { Build_plan.pool } drv

  module EvalC = Current_cache.Make(Eval)

  let eval ?pool expr =
    EvalC.get { Eval.pool } expr

  module ShellC = Current_cache.Make(Shell)

  let shell ?pool expr path =
    ShellC.get { Shell.pool } { expr; path }
end

module Nix = struct
  let pp_sp_label = Fmt.(option (prefix sp string))

  let rec realise ?label ?pool drv =
    let plan = Current.component "plan%a" pp_sp_label label |>
      let> drv = drv in
      Raw.build_plan ?pool drv
    in

    let self_build = plan |> Current.bind (fun Build_plan.Value.{ self; _ } ->
      Current.component "nix-store --realise%a" pp_sp_label label |>
        (* let> _deps_built = Current.list_iter (List.map build_dep deps) in *)
        (* OK I'm really confused by let>, need to investigate this is odd *)
        let> self = Current.return self in (* what a strange thing to write *)
        Raw.realise_one ?pool self
    ) in

    let deps = plan |> Current.map (fun Build_plan.Value.{ deps; _ } -> deps ) in
    let deps_module = (module struct
      (* include Realise.Key *)
      open Realise
      open Key
      type t = Key.t
      let compare = Key.compare
      let pp f { drv_path } =
        let desc = String.index_opt drv_path '-' |> Option.fold ~none:drv_path ~some:(fun i ->
          String.sub drv_path (i+1) (String.length drv_path - (i + 1))
        ) in
        Fmt.string f desc
    end : Current_term.S.ORDERED with type t = Realise.Key.t) in

    deps |> Current.bind ~info:(Current.component "deps%a" pp_sp_label label) (function
      | [] -> self_build
      | deps ->
        let deps_built = Current.return deps |> Current.list_iter deps_module (realise ?pool) in
        Current.gate ~on:deps_built self_build
    )
    
  let eval ?label ?pool expr =
    Current.component "nix-instantiate%a" pp_sp_label label |>
    let> expr = expr in
    Raw.eval ?pool expr

  let shell_result ?label ?pool expr path =
    Current.component "nix-shell%a" pp_sp_label label |>
    let> expr = expr in
    Raw.shell ?pool expr path

  let fetchgit ?label ?pool commit =
    (* NOTE this is efficient on 1 host (.git cloned once and re-fetched as needed,
     * but wasteful on multi-host (it's only needed on the host that's doing the instantiate)
     *)
    let expr = commit |> Current.map (fun commit ->
      "builtins.fetchGit {\n"
      ^ "url = \""^ (Git.Commit_id.repo commit) ^ "\";\n"
      ^ "rev = \""^ (Git.Commit_id.hash commit) ^ "\";\n"
      ^ "}"
    ) in

    let fetched = (Current.component "fetchgit%a" pp_sp_label label |>
      let> expr = expr in
      Raw.eval ?pool expr
    ) in
    (* We want the side effect of fetching, but don't need the result. There's probably a better way *)
    Current.pair fetched expr |> Current.map snd
end

module Opam2nix = struct
  (* not very typesafe, eh *)
  type resolve = {
    repo_commit: Git.Commit_id.t;
    nixpkgs: string;
    ocaml_version: string;
    opam2nix: string;
    package: string;
  }

  (* let resolve { nixpkgs; opam2nix; repo_commit; ocaml_version; package } =
    let expr =
      let> nixpkgs = nixpkgs
      and> opam2nix = opam2nix
      and> repo_commit = repo_commit
      and> ocaml_version = ocaml_version
      and> package = package in
      "(import ("^nixpkgs^") {})."^package
    in
    let drv = Nix.shell_result ~label:"opam2nix-resolve" expr (Fpath.of_string "opam2nix-selection.nix") in
    Nix.realise drv *)
end

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~github () : unit Current.t =
  let head_commit repo = Github.Api.head_commit github repo
      |> Current.map Github.Api.Commit.id in
  let nixpkgs = Nix.fetchgit ~label:"nixpkgs" (head_commit {owner = "nixos"; name = "nixpkgs" }) in
  let opam2nix_expr = Nix.fetchgit ~label:"opam2nix" (head_commit {owner = "timbertson"; name = "opam2nix" }) in
  let _opam2nix_impl =
    let expr = Current.pair nixpkgs opam2nix_expr |> Current.map (fun (nixpkgs, opam2nix) ->
      "(import ("^opam2nix^") { pkgs = ("^nixpkgs^"); })"
    ) in
    let drv = Nix.eval ~label:"opam2nix" expr in
    Nix.realise drv
  in

  let _repo_commit = head_commit {owner = "ocaml"; name = "opam-repository" } in
  
  let build ~label expr =
    let drv = Nix.eval ~label expr in
    Nix.realise drv
  in
  let buildpkg attr = build ~label:attr (nixpkgs |> Current.map (fun nixpkgs ->
      "(import ("^nixpkgs^") {})."^attr
    ))
  in
  
  Current.all [
    buildpkg "hello";
    buildpkg "ocaml-ng.ocamlPackages_4_10.ocaml";
    build ~label:"yojson" (
      Current.return "((import /home/tim/dev/ocaml/opam2nix/examples/package/generic.nix) {}).selection.yojson"
    )
  ]

let main config mode github =
  let engine = Current.Engine.create ~config (pipeline ~github) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ Current_github.Api.cmdliner),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
