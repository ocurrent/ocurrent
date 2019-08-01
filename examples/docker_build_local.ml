(* Example CI that tests OCurrent itself. *)

open Current.Syntax

module Git = Current_git
module Docker = Current_docker.Default

let pull = false    (* Whether to check for updates using "docker build --pull" *)

let () = Logging.init ()

(* Check that building OCurrent using the Dockerfile in the source works. This installs all dependencies at once
   and runs all tests, but cannot check that each plugin specifies all of its dependencies. *)
let build_and_test_with_dockerfile src =
  let image = Docker.build ~label:"Dockerfile" ~pull (`Git src) in
  Docker.run image ~args:["dune"; "exec"; "--"; "examples/docker_build_local.exe"; "--help"]

let monthly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 31) ()

(* Docker COPY can't cope with multiple directories, so do them one at a time. *)
let copy_dirs ~chown ~src dst =
  let open Dockerfile in
  empty @@@ (
    src |> List.map (fun src -> Dockerfile.copy ~src:["--chown=" ^ chown; src] ~dst:(Filename.concat dst src) ())
  )

(* Install one opam package:
   - copy "$name.opam"
   - install dependencies
   - copy sources
   - build, test and install package. *)
let install_package ~src name =
  let open Dockerfile in
  add ~src:["--chown=opam"; name ^ ".opam"] ~dst:"/src/" () @@
  run "opam install -y --deps-only -t ./%s.opam" name @@
  copy_dirs ~chown:"opam" ~src "./" @@
  run "opam install -tv ./%s.opam" name

(* Build just the core opam package, with only its opam dependencies available. *)
let build_core ~base ~src =
  let dockerfile =
    let+ base = base in
    let open Dockerfile in
    from (Docker.Image.hash base) @@
    run "sudo apt-get update && sudo apt-get install m4 pkg-config libsqlite3-dev -y --no-install-recommends" @@
    run "git pull origin master && git reset --hard f372039db86a970ef3e662adbfe0d4f5cd980701 && opam update" @@
    workdir "/src" @@
    run "sudo chown opam /src && mkdir plugins" @@
    install_package "current" ~src:["dune-project"; "lib_term"; "lib"; "lib_cache"; "plugins/fs"; "test"]
  in
  Docker.build ~label:"core" ~pull:false (`Git src) ~dockerfile

(* Build a single plugin's opam package, with only its opam dependencies available. *)
let build_plugin ~base ~src plugin =
  let dockerfile =
    let+ base = base in
    let open Dockerfile in
    from (Docker.Image.hash base) @@
    install_package ("current_" ^ plugin) ~src:["plugins/" ^ plugin]
  in
  Docker.build ~label:plugin ~pull:false (`Git src) ~dockerfile

(* Build the web UI's opam package, with only its opam dependencies available. *)
let build_web ~base ~src =
  let dockerfile =
    let+ base = base in
    let open Dockerfile in
    from (Docker.Image.hash base) @@
    install_package "current_web" ~src:["lib_web"]
  in
  Docker.build ~label:"web" ~pull:false (`Git src) ~dockerfile

(* Build everything together, and the plugins individually via opam. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let with_dockerfile = build_and_test_with_dockerfile src in
  let base = Docker.pull ~schedule:monthly "ocaml/opam2:4.08" in
  let core = build_core ~base ~src in
  let slack = build_plugin ~base:core ~src "slack" in
  let git = build_plugin ~base:core ~src "git" in
  let docker = build_plugin ~base:git ~src "docker" in
  let web = build_web ~base:core ~src in
  Current.all [
    with_dockerfile;
    Current.ignore_value @@ slack;
    Current.ignore_value @@ docker;
    Current.ignore_value @@ web;
  ]

let main config mode repo =
  let repo = Git.Local.v (Fpath.v repo) in
  let engine = Current.Engine.create ~config (pipeline ~repo) in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let repo =
  Arg.value @@
  Arg.pos 0 Arg.dir (Sys.getcwd ()) @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Build the head commit of a local Git repository using Docker." in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo),
  Term.info "docker_build_local" ~doc

let () = Term.(exit @@ eval cmd)
