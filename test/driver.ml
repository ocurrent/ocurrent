open Lwt.Infix
open Current.Syntax

exception Expect_skip

let () =
  Printexc.record_backtrace true

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

let init_logging () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Info));
  Logs.set_reporter reporter

module Git = Current_git_test
module Docker = Current_docker_test

let with_analysis ~name ~i (t : unit Current.t) =
  let data =
    let+ a = Current.Analysis.get t in
    Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp a);
    let url _ = None in
    Fmt.strf "%a" (Current.Analysis.pp_dot ~url) a
  in
  let path = Current.return (Fpath.v (Fmt.strf "%s.%d.dot" name !i)) in
  let* () = Current_fs.save path data in
  t

let current_watches = ref []

let cancel msg =
  let name w = Fmt.strf "%a" Current.Engine.pp_metadata w in
  match List.find_opt (fun w -> name w = msg) !current_watches with
  | None -> Fmt.failwith "No such watch %S." msg
  | Some w ->
    match (Current.Engine.actions w)#cancel with
    | Some c -> c ()
    | None -> Fmt.failwith "Watch %S cannot be cancelled" msg

let ready i = Current.Engine.is_stale i

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ?config ~name v actions =
  Git.reset ();
  Docker.reset ();
  (* Perform an initial analysis: *)
  let i = ref 1 in
  let trace x watches =
    current_watches := watches;
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.unit "()")) x);
    Logs.info (fun f -> f "@[<v>Depends on: %a@]" Fmt.(Dump.list Current.Engine.pp_metadata) watches);
    begin
      let ready_watch = List.find_opt ready watches in
      try
        actions !i;
        match ready_watch with
        | Some i -> Fmt.failwith "Input already ready! %a" Current.Engine.pp_metadata i
        | None -> ()
      with
      | Expect_skip -> assert (ready_watch <> None)
      | Exit ->
        List.iter (fun w -> (Current.Engine.actions w)#release) watches;
        raise Exit
    end;
    incr i;
    Lwt.pause () >|= fun () ->
    if not (List.exists ready watches) then failwith "No inputs ready (tests stuck)!"
  in
  let engine =
    Current.Engine.create ?config ~trace @@ fun () ->
    with_analysis ~name ~i @@ v ()
  in
  Lwt.catch
    (fun () -> Current.Engine.thread engine)
    (function
      | Exit -> Docker.assert_finished (); Lwt.return_unit
      | ex -> Lwt.fail ex
    )

let test_case_gc name fn =
  Alcotest_lwt.test_case name `Quick (fun switch () ->
      let old_errors = Logs.err_count () in
      fn switch () >|= fun () ->
      Gc.full_major ();
      Alcotest.(check int) "No errors logged" 0 @@ Logs.err_count () - old_errors
    )
