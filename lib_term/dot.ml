let list_bind f x =
  List.map f x |> List.concat

let pp_option f (name, v) =
  Fmt.pf f "%s=%S" name v

let pp_options ~sep f options =
  Fmt.(list ~sep pp_option) f options

let filtered_options options =
  options |> list_bind (function
      | _, None -> []
      | k, Some v -> [k, v]
  )

let pp_options_attr_list f options =
  let options = filtered_options options in
  match options with
  | [] -> ()
  | _ -> Fmt.pf f " [%a]" (pp_options ~sep:(Fmt.any ",")) options

let pp_options_stmts f options =
  let options = filtered_options options in
  Fmt.pf f "%a" (pp_options ~sep:(Fmt.any ";")) options


(* Graphviz generates invalid XML if the URL contains an ampersand. *)
let fix_escaping s =
  if not (String.contains s '&') then s
  else (
    let b = Buffer.create (String.length s * 2) in
    let rec aux i =
      match String.index_from_opt s i '&' with
      | None -> Buffer.add_substring b s i (String.length s - i)
      | Some j ->
        Buffer.add_substring b s i (j - i);
        Buffer.add_string b "&amp;";
        aux (j + 1)
    in
    aux 0;
    Buffer.contents b
  )

let limit_str len s =
  if String.length s <= len then s
  else String.sub s 0 (len - 3) ^ "..."

let node f ?style ?shape ?bg ?url ?tooltip i label =
  let url = Option.map fix_escaping url in
  let tooltip = Option.map (limit_str 4096) tooltip in (* (Graphviz max length is 16384) *)
  let attrs = [
    "label", Some label;
    "fillcolor", bg;
    "style", style;
    "shape", shape;
    "URL", url;
    "tooltip", tooltip;
    "target", (if url = None then None else Some "_top");
  ]
  in
  Fmt.pf f "n%d%a@," i pp_options_attr_list attrs

let edge f ?style ?color a b =
  let styles = [
    "style", style;
    "color", color;
  ]
  in
  Fmt.pf f "n%d -> n%d%a@," a b pp_options_attr_list styles

let begin_cluster f ?label i =
  let attrs = [
    (* We need to set the label explicitly if none is set by the caller,
     * because labels are inherited by subgraphs otherwise. *)
    "label", if label = None then Some "" else label;
  ]
  in
  Fmt.pf f "subgraph cluster_%d {%a@," i pp_options_stmts attrs

let end_cluster f =
  Fmt.pf f "}@,"
