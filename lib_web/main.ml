open Tyxml.Html

let html_to_string = Fmt.to_to_string (Tyxml.Html.pp ())

let string_of_watches = Fmt.to_to_string Current.Engine.pp_metadata

let render_watches w =
  li [txt (string_of_watches w)]

let render_job (id, _) =
  li [txt (Fmt.to_to_string Current.Job.pp_id id)]

let render_result = function
  | Ok () -> [txt "Success!"]
  | Error `Pending -> [txt "Pending..."]
  | Error (`Msg msg) -> [txt ("ERROR: " ^ msg)]

let template contents =
  html_to_string (
    html
      (head (title (txt "OCurrent")) [
          link ~rel:[ `Stylesheet ] ~href:"/css/style.css" ();
        ]
      )
      (body [
          h1 [a ~a:[a_href "/"] [txt "OCurrent"]];
          div ~a:[a_id "main"] contents
        ]
      )
  )

let dashboard { Current.Engine.value; analysis = _; watches; jobs } =
  template [
    div [
      object_ ~a:[a_data "/pipeline.svg"] [txt "Pipeline diagram"];
    ];
    h2 [txt "Result"];
    p (render_result value);
    h2 [txt "Jobs"];
    ul (List.map render_job (Current.Job_map.bindings jobs));
    h2 [txt "Watches"];
    ul (List.map render_watches watches);
  ]
