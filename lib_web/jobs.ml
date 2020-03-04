open Tyxml.Html

module Job = Current.Job

let render_row (id, job) =
  let url = Fmt.strf "/job/%s" id in
  let start_time =
    match Lwt.state (Job.start_time job) with
    | Lwt.Sleep -> "(ready to start)"
    | Lwt.Return t -> Query.string_of_timestamp (Unix.gmtime t)
    | Lwt.Fail f -> Printexc.to_string f
  in
  tr [
    td [ a ~a:[a_href url] [txt id] ];
    td [ txt start_time ];
  ]

let render () =
  let jobs = Current.Job.jobs () in
  Main.template (
    if Current.Job_map.is_empty jobs then [
      txt "There are no active jobs."
    ] else [
      table ~a:[a_class ["table"]]
        ~thead:(thead [
            tr [
              th [txt "Job"];
              th [txt "Start time"];
            ]
          ])
        (Current.Job_map.bindings jobs |> List.map render_row)
    ]
  )
