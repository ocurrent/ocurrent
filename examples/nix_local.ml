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
  
  let copy_to_log ~job src =
    let rec aux () =
      Lwt_io.read ~count:4096 src >>= function
      | "" -> Lwt.return_unit
      | data -> Current.Job.write job data; aux ()
    in
    aux ()
    
  module StringSet = Set.Make(String)

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let references_cmd = [ "nix-store"; "--query"; "--references"; key.Key.drv_path ] in
    let dry_run_cmd = [ "nix-store"; "--realise"; "--dry-run"; key.Key.drv_path ] in
    
    let lines ~desc channel =
      Lwt_io.read_lines channel
      |> Lwt_stream.map (fun line ->
        Current.Job.log job "[%s] %s" desc line;
        String.trim line
      )
      |> Lwt_stream.filter (fun line -> not (String.equal line ""))
      |> Lwt_stream.to_list
    in

    (* TODO could run in parallel *)
    Current.Process.exec_with ~cancellable:true ~job ("", dry_run_cmd |> Array.of_list) (fun proc ->
      (* can't use check_output since dry-run outputs to stderr *)
      let copy_thread = copy_to_log ~job proc#stdout in
      let result = lines ~desc:"dry-run" proc#stderr |> Lwt.map Result.ok in
      copy_thread >>= fun () -> result
    ) >>= fun dry_run ->
    Current.Process.exec_with ~cancellable:true ~job ("", references_cmd |> Array.of_list) (fun proc ->
      (* can't use check_output since dry-run outputs to stderr *)
      let copy_thread = copy_to_log ~job proc#stderr in
      let result = lines ~desc:"references" proc#stdout |> Lwt.map Result.ok in
      copy_thread >>= fun () -> result
    ) >>= fun references ->
    (* TODO ugly *)
    Lwt.return (Result.bind references (fun references ->
      dry_run |> Result.map (fun dry_run ->
        let intersection = StringSet.inter
          (StringSet.of_list references)
          (StringSet.of_list dry_run)
          |> StringSet.elements
          |> List.sort String.compare
        in

        Current.Job.log job "Found required dependencies: %a" (Fmt.list Fmt.string) intersection;
        intersection |> List.map Drv.unmarshal
      )
    ))

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
  
  (* TODO rename deps *)
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
    let deps = Current.component "plan%a" pp_sp_label label |>
      let> drv = drv in
      Raw.build_plan ?pool drv
    in

    let self_build drv = Current.component "build%a" pp_sp_label label |>
      (* let> _deps_built = Current.list_iter (List.map build_dep deps) in *)
      (* OK I'm really confused by let>, need to investigate this is odd *)
      let> drv = drv in
      Raw.realise_one ?pool drv
    in

    let deps_module = (module struct
      type t = Drv.t
      let compare = Drv.compare
      let pp = Drv.pp_short
    end : Current_term.S.ORDERED with type t = Drv.t) in

    deps |> Current.bind ~info:(Current.component "deps%a" pp_sp_label label) (function
      | [] -> self_build drv
      | deps ->
        let deps_built = Current.return deps |> Current.list_iter deps_module (realise ?pool) in
        self_build (Current.gate ~on:deps_built drv)
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

  let resolve { nixpkgs; opam2nix; repo_commit; ocaml_version; package } =
    let api =
      let> nixpkgs = nixpkgs
      and> opam2nix = opam2nix
      and> repo_commit = repo_commit
      and> ocaml_version = ocaml_version
      and> package = package in
      "(import ("^opam2nix^") {\n"
      ^ "nixpkgs = import ("^nixpkgs^") {};\n"
      ^ "}).make {"
    in
    let drv = Nix.eval ~label:"opam2nix" opam2nix_expr in
    let impl = Nix.realise drv in
    let solution_nix = Nix.run impl "bin/opam2nix"
      [
      "--resolve";
      "--ocaml-version"; "4.10.0";
      "--ocaml-attr"; "ocaml-ng.ocamlPackages_4_10.ocaml";
      "--output"; "/dev/stdout";
      "lwt"
      ]
    in
    let full_expression = 
    Nix.realise eval solution_nix
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
