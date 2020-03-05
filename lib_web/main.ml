open Tyxml.Html

let csrf_token =
  Random.self_init ();
  Random.int64 Int64.max_int |> Int64.to_string

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let render_result = function
  | Ok () -> [txt "Success!"]
  | Error (`Active `Ready) -> [txt "Ready..."]
  | Error (`Active `Running) -> [txt "Running..."]
  | Error (`Msg msg) -> [txt ("ERROR: " ^ msg)]

let template contents =
  html_to_string (
    html
      (head (title (txt "OCurrent")) [
          link ~rel:[ `Stylesheet ] ~href:"/css/style.css" ();
          meta ~a:[a_charset "UTF-8"] ();
        ]
      )
      (body [
          nav [
            ul [
              li [a ~a:[a_href "/"] [txt "OCurrent"]];
              li [a ~a:[a_href "/"] [txt "Home"]];
              li [a ~a:[a_href "/jobs"] [txt "Jobs"]];
              li [a ~a:[a_href "/query"] [txt "Query"]];
              li [a ~a:[a_href "/log-rules"] [txt "Log analysis"]];
              li [a ~a:[a_href "/heap"] [txt "Heap"]];
            ]
          ];
          div ~a:[a_id "main"] contents
        ]
      )
  )

let settings config =
  let selected = Current.Config.get_confirm config in
  let levels =
    Current.Level.values
    |> List.map @@ fun level ->
    let s = Current.Level.to_string level in
    let msg = Fmt.strf "Confirm if level >= %s" s in
    let sel = if selected = Some level then [a_selected ()] else [] in
    option ~a:(a_value s :: sel) (txt msg)
  in
  form ~a:[a_action "/set/confirm"; a_method `Post] [
    select ~a:[a_name "level"] (
      let sel = if selected = None then [a_selected ()] else [] in
      option ~a:(a_value "none" :: sel) (txt "No confirmation required") :: List.rev levels
    );
    input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf_token] ();
    input ~a:[a_input_type `Submit; a_value "Submit"] ();
  ]

let dashboard engine =
  let config = Current.Engine.config engine in
  let { Current.Engine.value; analysis = _; jobs = _ } = Current.Engine.state engine in
  template [
    div [
      object_ ~a:[a_data "/pipeline.svg"] [txt "Pipeline diagram"];
    ];
    h2 [txt "Result"];
    p (render_result value);
    h2 [txt "Settings"];
    settings config;
  ]
