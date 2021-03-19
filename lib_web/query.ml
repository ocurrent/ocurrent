open Tyxml.Html
module Db = Current_cache.Db

let render_value = function
  | Ok _ -> txt "OK"
  | Error (`Msg m) -> span ~a:[a_class ["error"]] [txt m]

let pp_duration f x =
  Fmt.(pf f "%a" uint64_ns_span (Int64.of_float (x *. 1.e9)))

let render_row ~jobs ~need_toggles { Db.job_id; build; value = _; rebuild; ready; running; finished; outcome } =
  let job = Fmt.str "/job/%s" job_id in
  let times =
    match running with
    | None ->
      Fmt.str "%a queued" pp_duration (finished -. ready)
    | Some running ->
      Fmt.str "%a/%a"
        pp_duration (running -. ready)
        pp_duration (finished -. running)
  in
  let cols = [
    td [ a ~a:[a_href job] [txt job_id] ];
    td [ txt (Int64.to_string build) ];
    td [ render_value outcome ];
    td [ txt (if rebuild then "Needs rebuild" else "-") ];
    td [ txt (Utils.string_of_timestamp (Unix.gmtime finished)) ];
    td [ txt times ];
  ] in
  if need_toggles then (
    let toggle =
      if Current.Job.Map.mem job_id jobs then
        [input ~a:[a_input_type `Checkbox; a_name "id"; a_value job_id] ()]
      else
        []
    in
    tr (td toggle :: cols)
  ) else (
    tr cols
  )

let bool_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some "true" -> Some true
  | Some "false" -> Some false
  | Some x -> Fmt.failwith "Invalid bool value %S in %a" x Uri.pp uri

let string_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some x -> Some x

let bool_table ~t ~f = [
  None,       "",      "(any)";
  Some true,  "true",  t;
  Some false, "false", f;
]

let enum_option ~choices name (value:string option) =
  let value = Option.value value ~default:"" in
  let choices = "" :: choices in
  select ~a:[a_name name] (
    choices |> List.map (fun form_value ->
        let sel = if form_value = value then [a_selected ()] else [] in
        let label = if form_value = "" then "(any)" else form_value in
        option ~a:(a_value form_value :: sel) (txt label)
      )
  )

let bool_option ?(t="True") ?(f="False") name value =
  select ~a:[a_name name] (
    bool_table ~t ~f |> List.map (fun (v, form_value, label) ->
        let sel = if v = value then [a_selected ()] else [] in
        option ~a:(a_value form_value :: sel) (txt label)
      )
  )

let string_option ~placeholder ~title name value =
  let value = Option.value value ~default:"" in
  input ~a:[a_name name; a_input_type `Text; a_value value; a_placeholder placeholder; a_title title] ()

let date_tip = "Actually, any prefix of the job ID can be used here."

let have_active_jobs ~jobs query_results =
  List.exists (fun entry -> Current.Job.Map.mem entry.Current_cache.Db.job_id jobs) query_results

let r ~engine = object
  inherit Resource.t

  val! can_get = `Viewer
  val! can_post = `Builder

  method! private get ctx =
    let uri = Context.uri ctx in
    let ok = bool_param "ok" uri in
    let rebuild = bool_param "rebuild" uri in
    let op = string_param "op" uri in
    let date = string_param "date" uri in
    let results = Db.query ?op ?ok ?rebuild ?job_prefix:date () in
    let ops = Db.ops () in
    let jobs = (Current.Engine.state engine).jobs in
    let need_toggles = have_active_jobs ~jobs results in
    let rebuild_form =
      if need_toggles then [
        input ~a:[a_input_type `Hidden; a_value (Context.csrf ctx); a_name "csrf"] ();
        input ~a:[a_input_type `Submit; a_value "Rebuild selected"] ();
      ] else []
    in
    let headings = [
      th [txt "Job"];
      th [txt "Build #"];
      th [txt "Result"];
      th [txt "Rebuild?"];
      th [txt "Finished"];
      th [txt "Queue/run time"];
    ] in
    let headings = if need_toggles then th [] :: headings else headings in
    Context.respond_ok ctx [
      form ~a:[a_action "/query"; a_method `Get] [
        ul ~a:[a_class ["query-form"]] [
            li [txt "Operation type:"; enum_option ~choices:ops "op" op];
            li [txt "Result:"; bool_option "ok" ok ~t:"Passed" ~f:"Failed"];
            li [txt "Needs rebuild:"; bool_option "rebuild" rebuild];
            li [txt "Date:"; string_option "date" date ~placeholder:"YYYY-MM-DD" ~title:date_tip];
            li [input ~a:[a_input_type `Submit; a_value "Submit"] ()];
          ];
      ];
      form ~a:[a_action "/query"; a_method `Post] (
        table ~a:[a_class ["table"]]
          ~thead:(thead [tr headings])
          (List.map (render_row ~jobs ~need_toggles) results) ::
        rebuild_form;
      )
    ]

  method! private post ctx body =
    let data = Uri.query_of_encoded body in
    let id = function
      | ("id", id) -> Some id
      | _ -> None
    in
    match List.filter_map id data |> List.concat with
    | [] -> Context.respond_error ctx `Bad_request "No jobs selected!"
    | jobs ->
      let failed = ref [] in
      jobs |> List.iter (fun job_id ->
          let state = Current.Engine.state engine in
          let jobs = state.Current.Engine.jobs in
          match Current.Job.Map.find_opt job_id jobs with
          | None -> failed := job_id :: !failed
          | Some actions ->
            match actions#rebuild with
            | None -> failed := job_id :: !failed
            | Some rebuild ->
              let _new_id : string = rebuild () in
              ()
        );
      match !failed with
      | [] -> Context.respond_redirect ctx (Uri.of_string "/query")
      | failed ->
        let msg =
          Fmt.str "%d/%d jobs could not be restarted (because they are no longer active): %a"
            (List.length failed) (List.length jobs)
            Fmt.(list ~sep:(any ", ") string) failed in
        Context.respond_error ctx `Bad_request msg

  method! nav_link = Some "Query"
end
