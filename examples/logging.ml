open Current.Syntax

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let init () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

let with_dot ~dotfile f () =
  let result = f () in
  let dot_data =
    let+ a = Current.Analysis.get result in
    Logs.debug (fun f -> f "Pipeline: @[%a@]" Current.Analysis.pp a);
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let* () = Current_fs.save (Current.return dotfile) dot_data in
  result
