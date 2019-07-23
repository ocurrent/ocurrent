open Tyxml.Html

let string_of_watch = Fmt.to_to_string Current.Engine.pp_metadata

let render_watch w =
  li [txt (string_of_watch w)]

let render_result = function
  | Ok () -> [txt "Success!"]
  | Error `Pending -> [txt "Pending..."]
  | Error (`Msg msg) -> [txt ("ERROR: " ^ msg)]

let render { Current.Engine.value; analysis = _; watches; jobs = _ } =
  html
    (head (title (txt "OCurrent")) [])
    (body [
        h1 [txt "OCurrent"];
        div [
          object_ ~a:[a_data "/pipeline.svg"] [txt "Pipeline diagram"];
        ];
        h2 [txt "Result"];
        p (render_result value);
        h2 [txt "Watches"];
        ul (List.map render_watch watches);
      ])
