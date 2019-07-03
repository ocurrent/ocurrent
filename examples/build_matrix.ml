open Current.Syntax

module Git = Current_git
module Docker = Current_docker

let () = Logging.init ()

let dockerfile ~base ~ocaml_version =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  run "opam switch %s" ocaml_version @@
  workdir "/src" @@
  add ~src:["*.opam"] ~dst:"/src/" () @@
  env ["OPAMERRLOGLEN", "0"] @@
  run "opam install . --show-actions --deps-only -t | awk '/- install/{print $3}' | xargs opam depext -iy" @@
  copy ~src:["."] ~dst:"/src/" () @@
  run "opam install -tv ."

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let base = Docker.pull ~schedule:weekly "ocaml/opam2" in
  let build ocaml_version =
    let dockerfile =
      let+ base = base in
      dockerfile ~base ~ocaml_version
    in
    Docker.build ~label:ocaml_version ~pull:false ~dockerfile src |>
    Docker.tag ~tag:(Fmt.strf "example-%s" ocaml_version)
  in
  Current.all [
    build "4.07";
    build "4.08"
  ]

(* Render pipeline as dot file *)
let dotfile = Fpath.v "pipeline.dot"

let main config repo =
  let repo = Git.Local.v (Fpath.v repo) in
  Lwt_main.run (Current.Engine.run ~config (Logging.with_dot ~dotfile (pipeline ~repo)))

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
  Term.(const main $ Current.Config.cmdliner $ repo),
  Term.info "build_matrix" ~doc

let () = Term.(exit @@ eval cmd)
