open Tyxml.Html
module Db = Current_cache.Db

let render_value = function
  | Ok _ -> txt "OK"
  | Error (`Msg m) -> span ~a:[a_class ["error"]] [txt m]

let pp_duration f x =
  Fmt.pf f "%.0fs" x

let render_row { Db.job_id; build; value = _; rebuild; ready; running; finished; outcome } =
  let job = Fmt.strf "/job/%s" job_id in
  let times =
    match running with
    | None ->
      Fmt.strf "%a queued" pp_duration (finished -. ready)
    | Some running ->
      Fmt.strf "%a+%a"
        pp_duration (running -. ready)
        pp_duration (finished -. running)
  in
  tr [
    td [ a ~a:[a_href job] [txt job_id] ];
    td [ txt (Int64.to_string build) ];
    td [ render_value outcome ];
    td [ txt (if rebuild then "Needs rebuild" else "-") ];
    td [ txt (Utils.string_of_timestamp (Unix.gmtime finished)) ];
    td [ txt times ];
  ]

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

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let uri = Context.uri ctx in
    let ok = bool_param "ok" uri in
    let rebuild = bool_param "rebuild" uri in
    let op = string_param "op" uri in
    let date = string_param "date" uri in
    let results = Db.query ?op ?ok ?rebuild ?job_prefix:date () in
    let ops = Db.ops () in
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
      table ~a:[a_class ["table"]]
        ~thead:(thead [
            tr [
              th [txt "Job"];
              th [txt "Build #"];
              th [txt "Result"];
              th [txt "Rebuild?"];
              th [txt "Finished"];
              th [txt "Queue/run time"];
            ]
          ])
        (List.map render_row results)
    ]

  method! nav_link = Some "Query"
end
