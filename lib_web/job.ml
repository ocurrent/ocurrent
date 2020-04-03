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
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Utils.csrf_token] () ]
      ]
  in
  let cancel_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.cancelled_state job = Ok () ->
      [form ~a:[action "cancel"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Cancel"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Utils.csrf_token] () ]
      ]
    | _ -> []
  in
  let start_button =
    match Current.Job.lookup_running job_id with
    | Some job when Current.Job.is_waiting_for_confirmation job ->
      [form ~a:[action "start"; a_method `Post]
         [ input ~a:[a_input_type `Submit; a_value "Start now"] ();
           input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Utils.csrf_token] () ]
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

type actions = <
  rebuild : (unit -> string) option;
>

let lookup_actions ~engine job_id =
  let state = Current.Engine.state engine in
  let jobs = state.Current.Engine.jobs in
  match Current.Job.Map.find_opt job_id jobs with
  | Some a -> (a :> actions)
  | None ->
    object
      method rebuild = None
    end

let job ~engine ~job_id = object
  inherit Resource.t

  method! private get _request =
    let actions = lookup_actions ~engine job_id in
    match Current.Job.log_path job_id with
    | Error (`Msg msg) -> Utils.respond_error `Bad_request msg
    | Ok path ->
      match Bos.OS.File.read path with
      | Error (`Msg msg) -> Utils.respond_error `Internal_server_error msg
      | Ok log ->
        let body = render ~actions ~job_id ~log in
        let headers =
          (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
          Cohttp.Header.init_with "X-Accel-Buffering" "no"
        in
        Utils.Server.respond ~status:`OK ~headers ~body ()
end

let rebuild ~engine ~job_id= object
  inherit Resource.t

  method! private post _request _body =
    let actions = lookup_actions ~engine job_id in
    match actions#rebuild with
    | None -> Utils.respond_error `Bad_request "Job does not support rebuild"
    | Some rebuild ->
      let new_id = rebuild () in
      Utils.Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ new_id)) ()
end

let cancel ~job_id = object
  inherit Resource.t

  method! private post _request _body =
    match Current.Job.lookup_running job_id with
    | None -> Utils.respond_error `Bad_request "Job does not support cancel (already finished?)"
    | Some job ->
      Current.Job.cancel job "Cancelled by user";
      Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
end

let start ~job_id = object
  inherit Resource.t

  method! private post _request _body =
    match Current.Job.lookup_running job_id with
    | None -> Utils.respond_error `Bad_request "Job is not awaiting confirmation"
    | Some j ->
      Current.Job.approve_early_start j;
      let id = Current.Job.id j in
      Utils.Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ id)) ()
end

let id ~date ~log = Fmt.strf "%s/%s" date log

let routes ~engine = Routes.[
    s "job" / str / str /? nil @--> (fun date log -> job ~engine ~job_id:(id ~date ~log));
    s "job" / str / str / s "rebuild" /? nil @--> (fun date log -> rebuild ~engine ~job_id:(id ~date ~log));
    s "job" / str / str / s "cancel" /? nil @--> (fun date log -> cancel ~job_id:(id ~date ~log));
    s "job" / str / str / s "start" /? nil @--> (fun date log -> start ~job_id:(id ~date ~log));
  ]
