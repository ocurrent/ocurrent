open Tyxml.Html

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let string_of_watches = Fmt.to_to_string Current.Engine.pp_metadata

let render_watches w =
  li [txt (string_of_watches w)]

let render_job (id, _) =
  li [txt (Fmt.to_to_string Current.Job.pp_id id)]

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
        ]
      )
      (body [
          nav [
            ul [
              li [a ~a:[a_href "/"] [txt "OCurrent"]];
              li [a ~a:[a_href "/"] [txt "Home"]];
              li [a ~a:[a_href "/query"] [txt "Query"]];
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
    input ~a:[a_input_type `Submit; a_value "Submit"] ();
  ]

let dashboard engine =
  let config = Current.Engine.config engine in
  let { Current.Engine.value; analysis = _; watches; jobs } = Current.Engine.state engine in
  template [
    div [
      object_ ~a:[a_data "/pipeline.svg"] [txt "Pipeline diagram"];
    ];
    h2 [txt "Result"];
    p (render_result value);
    h2 [txt "Settings"];
    settings config;
    h2 [txt "Debugging"];
    details (summary [txt "Debugging information"]) [
      h2 [txt "Jobs"];
      ul (List.map render_job (Current.Job_map.bindings jobs));
      h2 [txt "Watches"];
      ul (List.map render_watches watches);
    ];
  ]
