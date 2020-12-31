open Tyxml.Html

module Job = Current.Job

let render_row (id, job) =
  let url = Fmt.str "/job/%s" id in
  let start_time =
    match Lwt.state (Job.start_time job) with
    | Lwt.Sleep -> "(ready to start)"
    | Lwt.Return t -> Utils.string_of_timestamp (Unix.gmtime t)
    | Lwt.Fail f -> Printexc.to_string f
  in
  tr [
    td [ a ~a:[a_href url] [txt id] ];
    td [ txt start_time ];
  ]

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let jobs = Current.Job.jobs () in
    Context.respond_ok ctx (
      if Current.Job.Map.is_empty jobs then [
        txt "There are no active jobs."
      ] else [
        table ~a:[a_class ["table"]]
          ~thead:(thead [
              tr [
                th [txt "Job"];
                th [txt "Start time"];
              ]
            ])
          (Current.Job.Map.bindings jobs |> List.map render_row)
      ]
    )

  method! nav_link = Some "Jobs"
end
