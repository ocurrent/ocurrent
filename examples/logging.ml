open Current.Syntax

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"

  let subsystem = "logs"

  let inc_messages =
    let help = "Total number of messages logged" in
    let c =
      Counter.v_labels ~label_names:[ "level"; "src" ] ~help ~namespace
        ~subsystem "messages_total"
    in
    fun lvl src ->
      let lvl = Logs.level_to_string (Some lvl) in
      Counter.inc_one @@ Counter.labels c [ lvl; src ]
end

let reporter =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let src = Logs.Src.name src in
    Metrics.inc_messages level src;
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kpf k Fmt.stdout ("%a %a @[" ^^ fmt ^^ "@]@.")
      Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let init ?(level=Logs.Info) () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some level);
  Logs.set_reporter reporter

let with_dot ~dotfile f () =
  let result = f () in
  let dot_data =
    let+ a = Current.Analysis.get result in
    Logs.debug (fun f -> f "Pipeline: @[%a@]" Current.Analysis.pp a);
    let url _ = None in
    Fmt.strf "%a" (Current.Analysis.pp_dot ~url) a
  in
  let* () = Current_fs.save (Current.return dotfile) dot_data in
  result

let run x =
  match Lwt_main.run x with
  | Ok () -> Ok ()
  | Error (`Msg m) as e ->
    Logs.err (fun f -> f "%a" Fmt.lines m);
    e
