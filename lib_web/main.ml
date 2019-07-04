open Tyxml.Html

let string_of_watch = Fmt.to_to_string Current.Input.pp_watch

let render_watch w =
  li [txt (string_of_watch w)]

let render_result = function
  | Ok () -> [txt "Success!"]
  | Error `Pending -> [txt "Pending..."]
  | Error (`Msg msg) -> [txt ("ERROR: " ^ msg)]

let render { Current.Engine.value; analysis = _; watches } =
  html
    (head (title (txt "OCurrent")) [])
    (body [
        h1 [txt "OCurrent"];
        div [
          img ~src:"/pipeline.svg" ~alt:"Pipeline diagram" ();
        ];
        h2 [txt "Result"];
        p (render_result value);
        h2 [txt "Watches"];
        ul (List.map render_watch watches);
      ])
