open Current.Syntax

[@@@ocaml.warning "-3"]

type t = {
  fqdn : string;
  ip : string;
} [@@deriving yojson]

[@@@ocaml.warning "+3"]

let equal = (=)
let compare = compare
let hash t = t.fqdn ^ "=" ^ t.ip
let digest { fqdn; ip } = Fmt.str "%s=%s" fqdn ip

let pp f { fqdn = _; ip } = Fmt.pf f "%s" ip

let pp_short f t =
  Fmt.string f @@ Astring.String.with_range ~len:6 (hash t)

let expand a =
  Current.component "list" |>
  let> a = a in
  Current.Primitive.const (
    String.split_on_char '\n' a.Dig.Value.result |>
    List.filter_map (fun s -> match (Scanf.sscanf s "%s %s %s %s %s" (fun _ _ _ rtype ip -> (rtype, ip))) with
      | "AAAA", ip -> Some { fqdn = a.Dig.Value.fqdn; ip = "[" ^ ip ^ "]" }
      | "A", ip -> Some { fqdn = a.Dig.Value.fqdn; ip }
      | _, _ -> None
      | exception Scanf.Scan_failure _ -> None
      | exception End_of_file -> None)
  )

let marshal t = to_yojson t |> Yojson.Safe.to_string
let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok x -> x
  | Error e -> failwith e

