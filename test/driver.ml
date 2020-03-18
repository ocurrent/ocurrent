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

module SVar = Current.Var(struct
    type t = string * unit Current.t
    let equal = (==)
    let pp f (test, _pipeline) = Fmt.pf f "%s" test
  end)
let selected = SVar.create ~name:"current-test" (Error (`Msg "no-test"))

module Git = Current_git_test
module Docker = Current_docker_test

let i = ref 1

let test_pipeline =
  Current.component "choose pipeline" |>
  let** name, t = SVar.get selected in
  let data =
    let+ a = Current.Analysis.get t in
    Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp a);
    let url _ = None in
    Fmt.strf "%a" (Current.Analysis.pp_dot ~url) a
  in
  let path = Current.return (Fpath.v (Fmt.strf "%s.%d.dot" name !i)) in
  let* () = Current_fs.save path data in
  t

let current_watches = ref { Current.Engine.
                            value = Error (`Active `Ready);
                            analysis = Current.Analysis.booting;
                            watches = [];
                            jobs = Current.Job_map.empty }

let job_id_of msg =
  let name w = Fmt.strf "%a" Current.Engine.pp_metadata w in
  let watches = (!current_watches).Current.Engine.watches in
  match List.find_opt (fun w -> name w = msg) watches with
  | None -> Fmt.failwith "No such watch %S." msg
  | Some md ->
    (* Check that the job is in the index too. *)
    match Current.Engine.job_id md with
    | None -> Fmt.failwith "Job %S does not have an ID!" msg
    | Some job_id -> job_id

let actions_of msg =
  let job_id = job_id_of msg in
  let jobs = (!current_watches).Current.Engine.jobs in
  match Current.Job_map.find_opt job_id jobs with
  | None -> Fmt.failwith "Job %S is not in the index! Have @[%a@]"
              job_id
              Fmt.(Dump.list string) (Current.Job_map.bindings jobs |> List.map fst)
  | Some actions -> actions

let cancel msg =
  let job_id = job_id_of msg in
  match Current.Job.lookup_running job_id with
  | Some job -> Current.Job.cancel job "Cancelled by user"
  | None -> Fmt.failwith "Watch %S cannot be cancelled" msg

let rebuild msg =
  match (actions_of msg)#rebuild with
  | None -> Fmt.failwith "Job %S cannot be rebuilt!" msg
  | Some rebuild -> rebuild () |> ignore

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ?config ~name v actions =
  Git.reset ();
  Docker.reset ();
  SVar.set selected (Ok (name, v ()));
  i := 1;
  let trace ~next step_result =
    if !i = 0 then raise Exit;
    current_watches := step_result;
    let { Current.Engine.watches; value = x; _} = step_result in
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.unit "()")) x);
    Logs.info (fun f -> f "@[<v>Depends on: %a@]" Fmt.(Dump.list Current.Engine.pp_metadata) watches);
    begin
      if Lwt.state next <> Lwt.Sleep then Fmt.failwith "Already ready, and nothing changed yet!";
      try actions !i with
      | Expect_skip -> ()
      | Exit ->
        SVar.set selected (Error (`Msg "test-over"));
        i := -1
    end;
    incr i;
    Lwt.pause () >|= fun () ->
    if Lwt.state next = Lwt.Sleep then failwith "No inputs ready (tests stuck)!"
  in
  let engine = Current.Engine.create ?config ~trace (fun () -> test_pipeline) in
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
