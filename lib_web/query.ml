open Tyxml.Html
module Db = Current_cache.Db

let render_value = function
  | Ok _ -> txt "OK"
  | Error (`Msg m) -> span ~a:[a_class ["error"]] [txt m]

let render_row { Db.job_id; build; value = _; rebuild; ready = _; running = _; finished; outcome } =
  let job = Fmt.strf "/job/%s" job_id in
  tr [
    td [ a ~a:[a_href job] [txt job_id] ];
    td [ txt (Int64.to_string build) ];
    td [ render_value outcome ];
    td [ txt (if rebuild then "Needs rebuild" else "-") ];
    td [ txt (Utils.string_of_timestamp (Unix.gmtime finished)) ];
  ]

let bool_param name uri =
  match Uri.get_query_param uri name with
  | None | Some "" -> None
  | Some "true" -> Some true
  | Some "false" -> Some false
  | Some x -> Fmt.failwith "Invalid bool value %S in %a" x Uri.pp uri

let bool_table = [
  None,       "",      "(any)";
  Some true,  "true",  "Passed";
  Some false, "false", "Failed";
]

let bool_option name value =
  select ~a:[a_name name] (
    bool_table |> List.map (fun (v, form_value, label) ->
        let sel = if v = value then [a_selected ()] else [] in
        option ~a:(a_value form_value :: sel) (txt label)
      )
  )

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let uri = Context.uri ctx in
    let ok = bool_param "ok" uri in
    let results = Db.query ?ok () in
    Context.respond_ok ctx [
      form ~a:[a_action "/query"; a_method `Get] [
        table [
          tr [th [txt "Result:"]; td [bool_option "ok" ok]];
        ];
        input ~a:[a_input_type `Submit; a_value "Submit"] ();
      ];
      table ~a:[a_class ["table"]]
        ~thead:(thead [
            tr [
              th [txt "Job"];
              th [txt "Build #"];
              th [txt "Result"];
              th [txt "Rebuild?"];
              th [txt "Finished"];
            ]
          ])
        (List.map render_row results)
    ]

  method! nav_link = Some "Query"
end
