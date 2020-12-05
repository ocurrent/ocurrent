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

module Instantiate = struct
  let id = "nix-instantiate"

  type t = {
    pool : unit Current.Pool.t option;
  }

  (* TODO this should track nixpkgs & opam2nix hashes *)
  module Key = struct
    type t = {
      nixpkgs: Git.Commit.t;
      attr: string;
    }

    let cmd t tmpdir =
      [ "nix-instantiate"; "--attr"; t.attr; Fpath.to_string tmpdir ]

    (* let pp_cmd = Fmt.(list ~sep:sp (quote string)) *)
    let pp f { attr; nixpkgs } = Fmt.string f "TODO"

    let digest { attr; nixpkgs } =
      Yojson.Safe.to_string @@ `Assoc [
        "attr", `String attr;
        "nixpkgs", `String (Git.Commit_id.digest (Git.Commit.id nixpkgs));
      ]
  end
  module Value = Realise.Key

  let build { pool } job key =
    let ( >>!= ) = Lwt_result.bind in
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    Git.with_checkout ?pool ~job key.Key.nixpkgs (fun tmpdir ->
      Current.Process.check_output ~cancellable:true ~job ("", Key.cmd key tmpdir |> Array.of_list)
    ) >>!= fun drv ->
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

  module InstantiateC = Current_cache.Make(Instantiate)

  let instantiate ?pool (nixpkgs: Git.Commit.t) attr =
    InstantiateC.get { Instantiate.pool } Instantiate.{ nixpkgs; attr }
end

module Nix = struct
  let pp_sp_label = Fmt.(option (prefix sp string))

  let realise ?label ?pool drv =
    Current.component "nix-store --realise%a" pp_sp_label label |>
    let> drv = drv in
    Raw.realise ?pool drv

  let instantiate ?label ?pool nixpkgs attr =
    Current.component "nix-instantiate%a" pp_sp_label label |>
    let> nixpkgs: Git.Commit.t = nixpkgs
    (* TODO why does let> ... in let> ... in not work? what even is this syntax? *)
    and> attr = attr in
    Raw.instantiate ?pool nixpkgs attr
end

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~github () : unit Current.t =
  let head = Github.Api.head_commit github Github.{owner = "nixos"; name = "nixpkgs" } in
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let build attr =
    let drv = Nix.instantiate ~label:attr src (Current.return attr) in
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
