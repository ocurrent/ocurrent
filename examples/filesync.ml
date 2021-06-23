let program_name = "filesync"

let () = Prometheus_unix.Logging.init ()

(* Watch a file and copy when there are changes to it *)
let sync ~src ~dst () =
  let read = Current_fs.File.read src in
  Current_fs.File.write dst read

let main config mode src dst =
  let src = Fpath.v src in
  let dst = Fpath.v dst in
  let engine = Current.Engine.create ~config (sync ~src ~dst) in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:program_name
      (Current_web.routes engine)
  in
  Lwt_main.run
    (Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

(* Command-line parsing *)

open Cmdliner

let src =
  Arg.(
    required
    @@ pos 0 (some file) None
    @@ info ~doc:"Source file." ~docv:"SRC" [])

let dst =
  Arg.(
    required
    @@ pos 1 (some string) None
    @@ info ~doc:"Destination file." ~docv:"DST" [])

let cmd =
  let doc = "Keep two files in sync." in
  ( Term.(
      const main $ Current.Config.cmdliner $ Current_web.cmdliner $ src $ dst),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
