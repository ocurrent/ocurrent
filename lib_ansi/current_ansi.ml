(* (based on https://github.com/moby/datakit/blob/master/ci/src/cI_web.ml) *)

open Astring

let max_escape_length = 20

type gfx_state = {
  bold : bool;
  fg : Escape_parser.colour;
  bg : Escape_parser.colour;
  reversed : bool;
}

type t = {
  mutable gfx_state : gfx_state;
  mutable buf : string;
}

let default_gfx_state = { bold = false; fg = `Default; bg = `Default; reversed = false }

let name_of_colour = function
  | `Default | `Rgb _ -> None
  | `Black -> Some "black"
  | `Blue -> Some "blue"
  | `Cyan -> Some "cyan"
  | `Green -> Some "green"
  | `Magenta -> Some "magenta"
  | `Red -> Some "red"
  | `White -> Some "white"
  | `Yellow -> Some "yellow"

let apply_ctrl state = function
  | `Bold -> { state with bold = true }
  | `NoBold -> { state with bold = false }
  | `FgCol fg -> { state with fg }
  | `BgCol bg -> { state with bg }
  | `Reverse -> { state with reversed = true }
  | `NoReverse -> { state with reversed = false }
  | `Italic | `NoItalic | `NoUnderline | `Underline ->
      state
  | `Reset -> default_gfx_state

let pp_attr attr ~sep f = function
  | [] -> ()
  | cls -> Fmt.(pf f " %s='%a'" attr (list ~sep string) cls)
let pp_class = pp_attr "class" ~sep:Fmt.(const string " ")
let pp_style = pp_attr "style" ~sep:Fmt.(const string "; ")

let with_style s txt =
  match s with
  | { bold = false; fg = `Default; bg = `Default; _ } -> txt
  | { bold; fg; bg; reversed } ->
      let bg, fg = if reversed then fg, bg else bg, fg in
      let cl ty = function
        | None when bold && ty = "fg" -> [ "fg-bright-white" ]
        | Some c when bold && ty = "fg" -> [ Printf.sprintf "fg-bright-%s" c ]
        | Some c -> [ Printf.sprintf "%s-%s" ty c ]
        | None -> []
      in
      let cls = if bold then [ "bold" ] else [] in
      let cls = cl "fg" (name_of_colour fg) @ cls in
      let cls = cl "bg" (name_of_colour bg) @ cls in
      let style = function
        | (`Rgb x, `Fg) -> [ Printf.sprintf "color: #%06x" x ]
        | (`Rgb x, `Bg) -> [ Printf.sprintf "background-color: #%06x" x ]
        | _ -> []
      in
      let style = style (fg, `Fg) @ style (bg, `Bg) in
      Fmt.str "<span%a%a>%s</span>" pp_class cls pp_style style txt

let create () = { gfx_state = default_gfx_state; buf = "" }

let process t data =
  let output = Buffer.create (String.length data * 2) in
  let add = Buffer.add_string output in
  let module Stream = Char_stream in
  let write (s, first, stop) =
    let data = String.with_range s ~first ~len:(stop - first) in
    add (Xml_print.encode_unsafe_char data |> with_style t.gfx_state)
  in
  let rec aux s =
    match Escape_parser.parse s with
    | `Literal i when Stream.equal i s -> `Done ""
    | `Literal i ->
        write Stream.(s -- i);
        aux i
    | `Incomplete when Stream.avail s >= max_escape_length ->
        add "<b>ESCAPE-TOO-LONG</b>";
        aux (Stream.skip s)
    | `Incomplete -> `Done (Stream.to_string s)
    | `Invalid i -> aux i
    | `Escape (`Reset, i) ->
        t.gfx_state <- default_gfx_state;
        aux i
    | `Escape (`Ctrl (`SelectGraphicRendition c), i) ->
        t.gfx_state <- List.fold_left apply_ctrl t.gfx_state c;
        aux i
  in
  let (`Done unprocessed) = aux (Stream.of_string (t.buf ^ data)) in
  t.buf <- unprocessed;
  Buffer.contents output

let css = Style.css
