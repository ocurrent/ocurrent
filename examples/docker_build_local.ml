open Current.Syntax

module Git = Current_git
module Docker = Current_docker

let dotfile = Fpath.v "pipeline.dot"

(* Configure logging *)

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

(* Command-line parsing *)

let repo =
  Git.Local.v @@ Fpath.v @@
  match Sys.argv with
  | [| _ |] -> Sys.getcwd ()
  | [| _; path |] -> path
  | _ -> Fmt.failwith "Usage: docker_build_local DIR"

(* Run "docker build" on the latest commit in Git repository [path]. *)
let pipeline () =
  let head = Git.Local.(commit_of_ref repo (head repo)) in
  let src = Git.fetch head in
  let+ _image = Docker.build src in
  ()

(* Render pipeline as dot file *)
let pipeline () =
  let result = pipeline () in
  let dot_data =
    let+ a = Current.Analysis.get result in
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let* () = Current_fs.save (Current.return dotfile) dot_data in
  result

(* Mainloop *)
let () =
  Lwt_main.run (Current.Engine.run pipeline)
