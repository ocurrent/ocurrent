type t = {
  id : string;
} [@@deriving yojson]

let v id = Ok { id }

let create id = { id }

let id t = t.id

let marshal t =
  to_yojson t |> Yojson.Safe.to_string

let unmarshal = function
  | "" -> Ok None
  | s -> Yojson.Safe.from_string s |> of_yojson |> Result.map Option.some

let pp f t = Fmt.(quote string) f t.id
