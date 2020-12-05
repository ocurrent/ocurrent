let program_name = "nix_local"

module Git = Current_git
module Github = Current_github

let () = Logging.init ()

open Lwt.Infix
open Current.Syntax
module S = Current_cache.S

module Realise = struct
  let id = "nix-store-realise"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = struct
    type t = {
      drv_path : string;
    }

    let cmd { drv_path } =
      [ "nix-store"; "--realise"; drv_path ]

    (* let pp_cmd = Fmt.(list ~sep:sp (quote string)) *)
    let pp f { drv_path } = Fmt.string f drv_path

    let digest { drv_path } =
      Yojson.Safe.to_string @@ `Assoc [
        "drv", `String drv_path;
      ]
      
    let marshal { drv_path } = drv_path

    let unmarshal drv_path = { drv_path }
  end

  module Value = Current.Unit

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    Current.Process.exec ~cancellable:true ~job ("", Key.cmd key |> Array.of_list)

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
    let ( >>!= ) = Lwt_result.bind in
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = [ "nix-instantiate"; "--expr"; key ] |> Array.of_list in
    Current.Process.check_output ~cancellable:true ~job ("", cmd) >>!= fun drv ->
    let drv = String.trim drv in
    Current.Job.log job "Resolved -> %S" drv;
    Lwt_result.return { Realise.Key.drv_path = drv }

  let pp = Key.pp

  let auto_cancel = true
end

module Raw = struct
  module RealiseC = Current_cache.Make(Realise)

  let realise ?pool drv =
    RealiseC.get { Realise.pool } drv

  module EvalC = Current_cache.Make(Eval)

  let eval ?pool expr =
    EvalC.get { Eval.pool } expr
end

module Nix = struct
  let pp_sp_label = Fmt.(option (prefix sp string))

  let realise ?label ?pool drv =
    Current.component "nix-store --realise%a" pp_sp_label label |>
    let> drv = drv in
    Raw.realise ?pool drv

  let eval ?label ?pool expr =
    Current.component "nix-instantiate%a" pp_sp_label label |>
    let> expr = expr in
    Raw.eval ?pool expr
    
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

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~github () : unit Current.t =
  let head_commit repo = Github.Api.head_commit github repo
      |> Current.map Github.Api.Commit.id in
  let nixpkgs = Nix.fetchgit ~label:"nixpkgs" (head_commit Github.{owner = "nixos"; name = "nixpkgs" }) in

  let build attr =
    let expr = nixpkgs |> Current.map (fun nixpkgs ->
      "(import ("^nixpkgs^") {})."^attr
    ) in
    let drv = Nix.eval ~label:attr expr in
    Nix.realise drv
  in
  
  Current.all [
    build "hello";
    build "ocaml-ng.ocamlPackages_4_10.ocaml";
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
