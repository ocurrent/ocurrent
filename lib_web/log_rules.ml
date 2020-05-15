open Tyxml.Html
open Lwt.Infix

module Server = Cohttp_lwt_unix.Server
module LM = Current.Log_matcher

let render_row { LM.pattern; report; score } =
  tr [
    td [ txt pattern ];
    td [ txt report ];
    td [ txt (string_of_int score) ];
  ]

let get_recent_jobs = lazy (
  let db = Lazy.force Current.Db.v in
  Sqlite3.prepare db "SELECT job_id FROM cache ORDER BY finished DESC LIMIT ?"
)

let dump_groups f groups =
  if Array.length groups = 0 then Fmt.string f "Missing match!"
  else (
    Fmt.pf f "%s" groups.(0);
    for i = 1 to Array.length groups - 1 do
      Fmt.pf f "@.\\%d : %s" i groups.(i);
    done
  )

let test_pattern pattern =
  let re = Re.Pcre.re pattern |> Re.compile in
  let recent_jobs = Lazy.force get_recent_jobs in
  let jobs = Current.Db.query recent_jobs Sqlite3.Data.[ INT 10000L ] in
  let n_jobs = List.length jobs in
  let i = ref 0 in
  jobs |> Lwt_list.filter_map_s (function
      | Sqlite3.Data.[ TEXT job_id ] ->
        begin
          if !i = 0 then (
            i := 100;
            Lwt_main.yield ()
          ) else (
            decr i;
            Lwt.return_unit
          )
        end >|= fun () ->
        begin match Current.Job.log_path job_id with
          | Ok path ->
            let log_data =
              let ch = open_in_bin (Fpath.to_string path) in
              Fun.protect
                (fun () -> really_input_string ch (in_channel_length ch))
                ~finally:(fun () -> close_in ch)
            in
            Re.exec_opt re log_data |> Option.map (fun g ->
                let text = Fmt.strf "@[<v>%a@]" dump_groups (Re.Group.all g) in
                job_id, text
              )
          | Error _ -> None
        end
      | row -> Fmt.failwith "Bad row from get_recent_jobs: %a" Current.Db.dump_row row
    )
  >|= fun results ->
  let open Tyxml.Html in
  match results with
  | [] -> [p [txt (Fmt.strf "New pattern doesn't match anything in last %d jobs" n_jobs)]]
  | results ->
    [
      p [txt (Fmt.strf "%d matches in last %d jobs:" (List.length results) n_jobs)];
      table ~a:[a_class ["table"; "log-rules"]]
        ~thead:(thead [
            tr [
              th [txt "Job"];
              th [txt "Match"];
            ]
          ])
        (results |> List.map @@ fun (job_id, text) ->
         let job = Fmt.strf "/job/%s" job_id in
         tr [
           td [ a ~a:[a_href job] [txt job_id] ];
           td [pre [txt text]]
         ]
        )
    ]

let pattern_hints =
  let open Tyxml.Html in
  p [
    txt "In patterns, use ";
    code [txt "()"]; txt " for match groups, ";
    code [txt "?+*"]; txt " to match zero-or-one times, one-or-more times, or zero-or-more times, and ";
    code [txt "[\\n]"]; txt " to match newlines."
  ]

let render ?msg ?test ?(pattern="") ?(report="") ?(score="") ctx =
  let rules = Current.Log_matcher.list_rules () in
  let message = match msg with
    | None -> []
    | Some msg -> [p [txt msg]]
  in
  begin match test with
    | None -> Lwt.return []
    | Some p -> test_pattern p
  end >>= fun test_results ->
  let csrf = Context.csrf ctx in
  Context.respond_ok ctx (message @ [
      form ~a:[a_action "/log-rules"; a_method `Post] [
        table ~a:[a_class ["table"; "log-rules"]]
          ~thead:(thead [
              tr [
                th [txt "Pattern (PCRE)"];
                th [txt "Report"];
                th [txt "Score"];
              ]
            ])
          (List.map render_row rules @
           [
             tr [
               td [ input ~a:[a_input_type `Text; a_name "pattern"; a_value pattern] () ];
               td [ input ~a:[a_input_type `Text; a_name "report"; a_value report] () ];
               td ~a:[a_class ["score"]] [ input ~a:[a_input_type `Text; a_name "score"; a_value score] () ];
             ]
           ]
          );
        input ~a:[a_input_type `Submit; a_name "test"; a_value "Test pattern"] ();
        input ~a:[a_input_type `Submit; a_name "add"; a_value "Add rule"] ();
        input ~a:[a_input_type `Submit; a_name "remove"; a_value "Remove rule"] ();
        input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] ();
      ]
    ] @ [pattern_hints] @ test_results)

let handle_post ctx data =
  let pattern = List.assoc_opt "pattern" data |> Option.value ~default:[] in
  let report = List.assoc_opt "report" data |> Option.value ~default:[] in
  let score = List.assoc_opt "score" data |> Option.value ~default:[] in
  if List.mem_assoc "remove" data then (
    match pattern with
    | [""] -> Server.respond_error ~body:"Pattern can't be empty" ()
    | [pattern] ->
        begin match LM.remove_rule pattern with
          | Ok () -> render ctx ~msg:"Rule removed"
          | Error `Rule_not_found -> render ctx ~msg:"Rule not found" ~pattern
        end
    | _ ->
      Server.respond_error ~body:"Bad form submission" ()
  ) else if List.mem_assoc "add" data then (
    match pattern, report, score with
    | [""], _, _ -> Server.respond_error ~body:"Pattern can't be empty" ()
    | _, [""], _ -> Server.respond_error ~body:"Report can't be empty" ()
    | _, _, [""] -> Server.respond_error ~body:"Score can't be empty" ()
    | [pattern], [report], [score] ->
      begin match Re.Pcre.re pattern with
        | exception _ -> Server.respond_error ~body:"Invalid PCRE-format pattern" ()
        | _ ->
          begin match Astring.String.to_int score with
            | Some score -> LM.add_rule { LM.pattern; report; score }; render ctx ~msg:"Rule added"
            | None -> Server.respond_error ~body:"Score must be an integer" ()
          end
      end
    | _ ->
      Server.respond_error ~body:"Bad form submission" ()
  ) else if List.mem_assoc "test" data then (
    match pattern, report, score with
    | [""], _, _ -> Server.respond_error ~body:"Pattern can't be empty" ()
    | [pattern], [report], [score] ->
      begin match Re.Pcre.re pattern with
        | exception _ -> Server.respond_error ~body:"Invalid PCRE-format pattern" ()
        | _ -> render ctx ~test:pattern ~pattern ~report ~score
      end
    | _ -> Context.respond_error ctx `Bad_request "Bad form submission"
  ) else (
    Context.respond_error ctx `Bad_request "Bad form submission"
  )

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx = render ctx

  method! private post ctx body =
    let data = Uri.query_of_encoded body in
    handle_post ctx data

  method! nav_link = Some "Log analysis"
end
