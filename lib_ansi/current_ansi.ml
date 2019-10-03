(* (based on https://github.com/moby/datakit/blob/master/ci/src/cI_web.ml) *)

open Astring

let max_escape_length = 20

type gfx_state = { bold : bool; fg : string option; bg : string option }

type t = {
  mutable gfx_state : gfx_state;
  mutable buf : string;
}

let default_gfx_state = { bold = false; fg = None; bg = None }

let format_colour = function
  | `Default -> None
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
  | `FgCol c -> { state with fg = format_colour c }
  | `BgCol c -> { state with bg = format_colour c }
  | `Italic | `NoItalic | `NoReverse | `NoUnderline | `Reverse | `Underline ->
      state
  | `Reset -> default_gfx_state

let pp_style = Fmt.(list ~sep:(const string " ")) Fmt.string

let with_style s txt =
  match s with
  | { bold = false; fg = None; bg = None } -> txt
  | { bold; fg; bg } ->
      let cl ty = function
        | None when bold && ty = "fg" -> [ "fg-bright-white" ]
        | Some c when bold && ty = "fg" -> [ Printf.sprintf "fg-bright-%s" c ]
        | Some c -> [ Printf.sprintf "%s-%s" ty c ]
        | None -> []
      in
      let style = if bold then [ "bold" ] else [] in
      let style = cl "fg" fg @ style in
      let style = cl "bg" bg @ style in
      Fmt.strf "<span class='%a'>%s</span>" pp_style style txt

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
