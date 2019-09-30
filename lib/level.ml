[@@@ocaml.warning "-32"]

type t =
  | Harmless
  | Mostly_harmless
  | Average
  | Above_average
  | Dangerous
[@@deriving ord, enum]

[@@@ocaml.warning "+32"]

let to_string = function
  | Harmless        -> "harmless"
  | Mostly_harmless -> "mostly-harmless"
  | Average         -> "average"
  | Above_average   -> "above-average"
  | Dangerous       -> "dangerous"

let pp = Fmt.of_to_string to_string

let values =
  let rec aux i =
    match of_enum i with
    | Some l -> l :: aux (i + 1)
    | None -> []
  in
  aux min

let of_string x =
  match List.find_opt (fun l -> to_string l = x) values with
  | Some x -> Ok x
  | None ->
    Error (`Msg (Fmt.strf "Unknown level %S; expected one of %a" x
                   Fmt.(list ~sep:(unit ", ") pp) values))
