module Git = Current_git
module Sandmark = Current_sandmark

let () = Logging.init ()

let pool = Current.Pool.create ~label:"benchmark" 1

(* Run [cmd] and return the first line it produces. *)
let pread cmd = 
  let ch = Unix.open_process_in cmd in
  let v = input_line ch in
  match Unix.close_process_in ch with
  | Unix.WEXITED 0 -> v
  | _ -> Fmt.failwith "Command %S failed!" cmd

let config =
  Sandmark.Config.v
    ~run_stages:"setup,bench,upload"                (* Split into OCurrent components? *)
    ~environment:"bench2.ocamllabs.io"
    ~codespeed_url:"http://bench2.ocamllabs.io:80/"
    ~arch:(pread "uname -m")
    ~bench_core:"5"

(* Benchmark recent commits on the branch with the given head. *)
let benchmark_branch ~config head =
  Sandmark.recent_commits ~n:5 head
  |> Current.list_iter ~pp:Git.Commit.pp_short (Sandmark.sandmark ~pool ~config)

(* Benchmark recent commits on all interesting branches. *)
let pipeline ~repo () =
  let benchmark ?main_branch ?sandmark_tag_override ?upload_project_name ~spec:executable_spec branch =
    ignore main_branch; (* What is this for? *)
    let config = config ?sandmark_tag_override ?upload_project_name ~executable_spec () in
    benchmark_branch ~config @@ Git.Local.commit_of_ref repo @@ Fmt.strf "refs/remotes/%s" branch
  in
  Current.all [
    benchmark "origin/4.08"                 ~spec:"vanilla";
    benchmark "origin/4.09"                 ~spec:"vanilla";
    benchmark "origin/master"               ~spec:"vanilla"      ~sandmark_tag_override:"5.10.0";
    benchmark "ocaml-multicore/master"      ~spec:"multicore:mc" ~main_branch:"master"      ~upload_project_name:"ocaml-multicore";
    benchmark "kayceesrk_ocaml/r14-globals" ~spec:"vanilla"      ~main_branch:"r14-globals" ~upload_project_name:"kc-r14-globals";
  ]

(* Run the pipeline and a simple web UI. *)
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
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The directory containing the .git subdirectory."
    ~docv:"DIR"
    []

let cmd =
  let doc = "Run OCaml compiler Sandmark benchmarks" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repo),
  Term.info "sandmark" ~doc

let () = Term.(exit @@ eval cmd)
