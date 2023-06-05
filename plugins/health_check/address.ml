open Current.Syntax

[@@@ocaml.warning "-3"]

type ipversion = V4 | V6

type t = {
  fqdn : string;
  ip : string;
  ver : ipversion [@to_yojson fun x -> match x with
                                       | V4 -> `String "IPv4"
                                       | V6 -> `String "IPv6" ]
                  [@of_yojson fun x -> match x with
                                       | `String "IPv4" -> Ok V4
                                       | `String "IPv6" -> Ok V6
                                       | _ -> failwith "Invalid JSON"];
} [@@deriving yojson]

[@@@ocaml.warning "+3"]

let equal = (=)
let compare = compare
let hash t = t.fqdn ^ "=" ^ t.ip
let digest { fqdn; ip } = Fmt.str "%s=%s" fqdn ip

let pp f { fqdn = _; ip } = Fmt.pf f "%s" ip

let pp_short f t =
  Fmt.string f @@ Astring.String.with_range ~len:6 (hash t)

let expand dns =
  Current.component "list" |>
  let> dns = dns in
  Current.Primitive.const (
    String.split_on_char '\n' dns.Dig.Value.result |>
    List.filter_map (fun s -> match (Scanf.sscanf s "%s %s %s %s %s" (fun _ _ _ rtype ip -> (rtype, ip))) with
      | "AAAA", ip -> Some { fqdn = dns.Dig.Value.fqdn; ip; ver = V6 }
      | "A", ip -> Some { fqdn = dns.Dig.Value.fqdn; ip; ver = V4 }
      | _, _ -> None
      | exception Scanf.Scan_failure _ -> None
      | exception End_of_file -> None)
  )

let marshal t = to_yojson t |> Yojson.Safe.to_string
let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok x -> x
  | Error e -> failwith e

