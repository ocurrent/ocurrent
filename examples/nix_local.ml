let program_name = "nix_local"

module Git = Current_git
module Github = Current_github

let () = Logging.init
  (* ~level:Logs.Debug *)
  ()

open Current.Syntax

module Nix = Current_nix.Remote

module Cluster = Current_ocluster

module S = Current_cache.S

module Log = struct
  let src = Logs.Src.create "nix" ~doc:"Nix support"
  include (val Logs.src_log src : Logs.LOG)
end

module Opam2nix = struct
  type t = {
    repo_commit: Git.Commit_id.t;
    nixpkgs: string;
    opam2nix: string;
    ocaml_version: string;
    ocaml_attr: string; (* TODO could be derived *)
    package: string;
    version: string;
  }

  let build ~ctx ?label t =
    (* TODO use same ocaml for opam2nix and package build? *)
    let prelude = t |> Current.map (fun { nixpkgs; opam2nix; _ } ->
        "let\n"
      ^ "pkgs = import ("^nixpkgs^") {};\n"
      ^ "opam2nix = (import ("^opam2nix^") {\n"
      ^   "inherit pkgs;\n"
      ^ "});\n"
      ^ "in\n"
    ) in

    let exe_drv = Nix.eval ~ctx ?label (prelude |> Current.map (fun expr -> expr ^ "opam2nix")) in

    let cmd = Current.pair exe_drv t |> Current.map (fun (drv, { repo_commit; ocaml_version; package; version; _ }) ->
      Nix.Exec.{
        drv;
        exe = "bin/opam2nix";
        (* TODO this can't be called concurrently on the same host *)
        args = [ "resolve";
          "--ocaml-version"; ocaml_version;
          "--repo-commit"; (Git.Commit_id.hash repo_commit);
          "--dest"; "/dev/stdout";
          package^"="^version;
        ]
      }
    ) in

    let selection_expr =
      Current.component "selection.nix" |>
      let** _ = Nix.build_drv ~ctx ?label exe_drv
      in Nix.exec ~ctx ?label cmd
    in

    let package_expr =
      let+ prelude = prelude
      and+ t = t
      and+ selection_expr = selection_expr
      in prelude
      ^ "(opam2nix.build {\n"
      ^   "ocaml = pkgs."^t.ocaml_attr^";\n"
      ^   "selection = ("^selection_expr^");\n"
      ^ "}).\"" ^ t.package ^ "\""
    in

    Nix.build_nix ~ctx ?label package_expr
end

let pipeline ~github ~local_pool ~cluster () : unit Current.t =
  let head_commit repo = Github.Api.head_commit github repo
      |> Current.map Github.Api.Commit.id in
  (* Hackity hack to get more cache hits locally *)
  let override_commit sha commit = commit |> Current.map (fun c ->
    let open Git.Commit_id in
    v ~repo:(repo c) ~gref:sha ~hash:sha
  ) in
  let nixpkgs = Nix.fetchgit ~label:"nixpkgs" (override_commit "cf7475d2061ac3ada4b226571a4a1bb91420b578" (head_commit {owner = "nixos"; name = "nixpkgs" })) in
  let opam2nix = Nix.fetchgit ~label:"opam2nix" (head_commit {owner = "timbertson"; name = "opam2nix" }) in
  let repo_commit = head_commit {owner = "ocaml"; name = "opam-repository" } in

  let pool = "x86_64-linux" in

  Current.all [
    Opam2nix.(build (
      let+ repo_commit = repo_commit
      and+ nixpkgs = nixpkgs
      and+ opam2nix = opam2nix
      in
      {
        repo_commit;
        nixpkgs;
        opam2nix;
        ocaml_version = "4.10.0";
        ocaml_attr = "ocaml-ng.ocamlPackages_4_10.ocaml";
        package = "lwt";
        version = "5.3.0";
      }) ~ctx:(Nix.ctx ~cluster ~pool ~local:local_pool)) |> Current.map ignore
  ]


(* Command-line parsing *)

open Cmdliner

let main config mode github cluster_cap =
  let or_die = function
    | Ok x -> x
    | Error `Msg m -> failwith m
  in

  let vat = Capnp_rpc_unix.client_only_vat () in
  let sr = Capnp_rpc_unix.Cap_file.load vat cluster_cap |> or_die in
  let conn = Current_ocluster.Connection.create sr in
  let cluster = Current_ocluster.v conn in
  let local_pool = Some (Current.Pool.create ~label:"nix-local" 1) in
  let engine = Current.Engine.create ~config (pipeline ~github ~local_pool ~cluster) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Ocluster submission.cap file"
    ~docv:"ADDR"
    ["c"; "connect"]

let cluster_cmdliner = Obj.magic ()

let cmd =
  let doc = "opam2nix! woo!" in
  Term.(const main
    $ Current.Config.cmdliner
    $ Current_web.cmdliner
    $ Current_github.Api.cmdliner
    $ connect_addr),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
