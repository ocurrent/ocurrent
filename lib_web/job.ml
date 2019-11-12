open Tyxml.Html
open Astring

let sep = "@@LOG@@"

let render ~actions ~job_id ~log =
  let ansi = Current_ansi.create () in
  let action op = a_action (Fmt.strf "/job/%s/%s" job_id op) in
  let rebuild_button =
    if actions#rebuild = None then []
    else
      [form ~a:[action "rebuild"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Rebuild"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Main.csrf_token] () ]
      ]
  in
  let cancel_button =
    if actions#cancel = None then []
    else
      [form ~a:[action "cancel"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Cancel"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Main.csrf_token] () ]
      ]
  in
  let start_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.is_waiting_for_confirmation job ->
      [form ~a:[action "start"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Start now"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Main.csrf_token] () ]
      ]
    | _ -> []
  in
  let tmpl =
    Main.template (
      rebuild_button @
      cancel_button @
      start_button @
      [pre [txt sep]]
    )
  in
  match String.cut ~sep tmpl with
  | None -> assert false
  | Some (pre, post) ->
    let i = ref `Pre in
    let stream =
      Lwt_stream.from_direct (fun () ->
          match !i with
          | `Pre -> i := `Log; Some pre
          | `Log -> i := `Post; Some (Current_ansi.process ansi log)
          | `Post -> i := `Done; Some post
          | `Done -> None
        )
    in
    Cohttp_lwt.Body.of_stream stream
