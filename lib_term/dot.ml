let list_bind f x =
  List.map f x |> List.concat

let pp_option f (name, v) =
  Fmt.pf f "%s=%S" name v

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
  ] |> list_bind (function
      | _, None -> []
      | k, Some v -> [k, v]
    )
  in
  Fmt.pf f "n%d [%a]@," i Fmt.(list ~sep:(unit ",") pp_option) attrs

let pp_options f = function
  | [] -> ()
  | items ->
    Fmt.pf f " [%a]"
      (Fmt.list ~sep:(Fmt.unit ",") pp_option) items

let edge f ?style ?color a b =
  let styles = [
    "style", style;
    "color", color;
  ] |> list_bind (function
      | _, None -> []
      | k, Some v -> [k, v]
    )
  in
  Fmt.pf f "n%d -> n%d%a@," a b pp_options styles

let begin_cluster f i =
  Fmt.pf f "subgraph cluster_%d {@," i

let end_cluster f =
  Fmt.pf f "}@,"
