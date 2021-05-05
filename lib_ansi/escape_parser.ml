module Stream = Char_stream

type colour =
  [ `Default | `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow | `Rgb of int ]

type sgr =
  [ `BgCol of colour
  | `Bold
  | `FgCol of colour
  | `Italic
  | `NoBold
  | `NoItalic
  | `NoReverse
  | `NoUnderline
  | `Reset
  | `Reverse
  | `Underline ]

type escape = [ `Reset | `Ctrl of [ `SelectGraphicRendition of sgr list ] ]

let is_param_byte c =
  let c = Char.code c in
  c land 0xf0 = 0x30

let is_im_byte c =
  let c = Char.code c in
  c land 0xf0 = 0x40

let is_final_byte c =
  let c = Char.code c in
  c >= 0x40 && c <= 0x7e

exception Unknown_escape

let colour = function
  | 0 -> `Black
  | 1 -> `Red
  | 2 -> `Green
  | 3 -> `Yellow
  | 4 -> `Blue
  | 5 -> `Magenta
  | 6 -> `Cyan
  | 7 -> `White
  | _ -> raise Unknown_escape

let sgr = function
  | 0 -> `Reset
  | 1 -> `Bold
  | 3 -> `Italic
  | 4 -> `Underline
  | 7 -> `Reverse
  | 22 -> `NoBold
  | 23 -> `NoItalic
  | 24 -> `NoUnderline
  | 27 -> `NoReverse
  | x when x >= 30 && x <= 37 -> `FgCol (colour (x - 30))
  | x when x >= 90 && x <= 97 -> `FgCol (colour (x - 90)) (* Non-standard "bright" fg colour *)
  | 39 -> `FgCol `Default
  | x when x >= 40 && x <= 47 -> `BgCol (colour (x - 40))
  | 49 -> `BgCol `Default
  | _ -> raise Unknown_escape

let sgrs params =
  match params with
  | "" :: _ -> [ `Reset ]
  | _ ->
    match List.map int_of_string params with
    | exception Failure _ ->
      raise Unknown_escape
    | params ->
      let rgb r g b = `Rgb (r lsl 16 lor g lsl 8 lor b) in
      let rec go = function
        | 38 :: 2 :: r :: g :: b :: rest ->
          `FgCol (rgb r g b) :: go rest
        | 48 :: 2 :: r :: g :: b :: rest ->
          `BgCol (rgb r g b) :: go rest
        | n :: rest ->
          sgr n :: go rest
        | [] -> []
      in
      go params

let parse_ctrl ~params = function
  | "m" -> `SelectGraphicRendition (sgrs params)
  | _ -> raise Unknown_escape

let read_intermediates ~params start =
  let rec aux s =
    match Stream.next s with
    | None -> `Incomplete (* No final byte *)
    | Some (x, s) when is_im_byte x -> aux s
    | Some (x, s2) when is_final_byte x -> (
        let func = Stream.(start -- s2 |> string_of_span) in
        let params = Astring.String.cuts ~sep:";" params in
        let params = List.concat_map (Astring.String.cuts ~sep:":") params in
        try `Escape (`Ctrl (parse_ctrl ~params func), s2)
        with Unknown_escape -> `Invalid s2 )
    | Some _ -> `Invalid s
  in
  aux start

let read_params start =
  let rec aux s =
    match Stream.next s with
    | None -> `Incomplete (* No final byte *)
    | Some (x, s) when is_param_byte x -> aux s
    | Some _ ->
        let params = Stream.(start -- s |> string_of_span) in
        read_intermediates ~params s
  in
  aux start

(* Parse [esc], an escape sequence. *)
let parse_escape esc =
  match Stream.(next (Stream.skip esc)) with
  | Some ('[', s) -> read_params s (* [esc] is a control sequence *)
  | Some (']', s) ->
      `Invalid s (* [esc] is a operating system command sequence (todo) *)
  | Some ('c', s) -> `Escape (`Reset, s)
  | Some (_, s) -> `Invalid s (* TODO: other types of escape *)
  | None -> `Incomplete

let parse input =
  (* In theory, we could also get the 8-bit escape character encoded as two
     UTF-8 bytes, but for now we just process the "<ESC>[" sequence, which
     seems to be what everyone is using. *)
  match Stream.find input '\x1b' with
  | None -> `Literal (Stream.skip_all input)
  | Some i when Stream.equal input i -> parse_escape input
  | Some i -> `Literal i
