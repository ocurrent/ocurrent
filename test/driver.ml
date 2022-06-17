open Lwt.Infix
open Current.Syntax

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
    type t = (unit -> unit Current.t)
    let equal = (==)
    let pp f _ = Fmt.string f "pipeline"
  end)
let selected = SVar.create ~name:"current-test" (Error (`Msg "no-test"))

module Git = Current_git_test
module Docker = Current_docker_test

let test_pipeline =
  Current.component "choose pipeline" |>
  let** make_pipeline = SVar.get selected in
  make_pipeline ()

let current_watches = ref { Current.Engine.
                            value = Error (`Active `Ready);
                            jobs = Current.Job.Map.empty }

let pp_job f j = j#pp f

let find_by_descr msg =
  let jobs = (!current_watches).Current.Engine.jobs |> Current.Job.Map.bindings in
  match List.find_opt (fun (_, job) -> Fmt.str "%t" job#pp = msg) jobs with
  | None ->
    Fmt.failwith "@[<v2>No job with description %S. We have:@,%a@]" msg
      Fmt.(Dump.list pp_job) (List.map snd jobs)
  | Some x -> x

let cancel msg =
  let job_id, _actions = find_by_descr msg in
  match Current.Job.lookup_running job_id with
  | Some job -> Current.Job.cancel job "Cancelled by user"
  | None -> Fmt.failwith "Watch %S cannot be cancelled" msg

let rebuild msg =
  let _job_id, actions = find_by_descr msg in
  match actions#rebuild with
  | None -> Fmt.failwith "Job %S cannot be rebuilt!" msg
  | Some rebuild -> rebuild () |> ignore

let stats =
  let pp f { Current_term.S.ok; ready; running; failed; blocked } =
    Fmt.pf f "ok=%d,ready=%d,running=%d,failed=%d,blocked=%d" ok ready running failed blocked
  in
  Alcotest.testable pp (=)

(* Write two SVG files for pipeline [v]: one containing the static analysis
   before it has been run, and another once a particular commit hash has been
   supplied to it. *)
let test ?config ?final_stats ~name v actions =
  Git.reset ();
  Docker.reset ();
  SVar.set selected (Ok v);
  let step = ref 1 in
  Current_incr.propagate ();
  let trace ~next step_result =
    if !step = 0 then raise Exit;
    begin
      Logs.info (fun f -> f "Analysis: @[%a@]" Current.Analysis.pp test_pipeline);
      let path = Fmt.str "%s.%d.dot" name !step in
      let ch = open_out path in
      let f = Format.formatter_of_out_channel ch in
      let collapse_link ~k:_ ~v:_ = None in
      let job_info { Current.Metadata.job_id = _; update } = update, None in
      let env = [] in
      Fmt.pf f "%a@!" (Current.Analysis.pp_dot ~env ~collapse_link ~job_info) test_pipeline;
      close_out ch
    end;
    current_watches := step_result;
    let { Current.Engine.value = x; _} = step_result in
    Logs.info (fun f -> f "--> %a" (Current_term.Output.pp (Fmt.any "()")) x);
    begin
      if Lwt.state next <> Lwt.Sleep then Fmt.failwith "Already ready, and nothing changed yet!";
      try actions !step with
      | Exit ->
        final_stats |> Option.iter (fun expected ->
            Alcotest.check stats "Check final stats" expected @@ Current.Analysis.quick_stat ();
            (* Alcotest.check stats "Check final stats" expected @@ Current.Analysis.stats test_pipeline *)
          );
        SVar.set selected (Error (`Msg "test-over"));
        step := -1
    end;
    incr step;
    let rec wait i =
      match i with
      | 0 -> failwith "No inputs ready (tests stuck)!"
      | i when Lwt.state next = Lwt.Sleep ->
        Lwt.pause () >>= fun () ->
        wait (i - 1)
      | _ -> Lwt.return_unit
    in
    wait 3      (* Wait a few turns for things to become ready *)
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
      fn switch () >>= fun () ->
      SVar.set selected (Error (`Msg "no-test"));
      Current_incr.propagate ();
      Lwt.pause () >>= fun () ->
      Gc.full_major ();
      Alcotest.(check int) "No errors logged" 0 @@ Logs.err_count () - old_errors;
      Prometheus.CollectorRegistry.(collect default) >|= fun data ->
      Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
      |> String.split_on_char '\n'
      |> List.iter (fun line ->
          if Astring.String.is_prefix ~affix:"ocurrent_cache_memory_cache_items{" line then (
            match Astring.String.cut ~sep:"} " line with
            | None -> Fmt.failwith "Bad metrics line: %S" line
            | Some (key, _) when Astring.String.is_infix ~affix:"_total{" key -> ()
            | Some (key, value) ->
              if float_of_string value <> 0.0 then
                Fmt.failwith "Non-zero metric after test: %s=%s" key value
          )
        );
    )
