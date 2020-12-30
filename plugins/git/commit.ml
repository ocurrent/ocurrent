[@@@ocaml.warning "-3"]

module Fpath = struct
  include Fpath
  let to_yojson x = [%to_yojson:string] (Fpath.to_string x)
  let of_yojson x =
    match [%of_yojson:string] x with
    | Ok x -> Ok (Fpath.v x)
    | Error _ as e -> e
end

type t = {
  repo : Fpath.t;
  id : Commit_id.t;
} [@@deriving yojson]

[@@@ocaml.warning "+3"]

let id t = t.id
let hash t = t.id.Commit_id.hash
let compare a b = String.compare (hash a) (hash b)
let equal a b = String.equal (hash a) (hash b)
let pp = Fmt.using hash Fmt.string

let pp_short f t =
  Fmt.string f @@ Astring.String.with_range ~len:6 (hash t)

let check_cached t =
  let hash = hash t in
  let branch = Fmt.str "fetch-%s" hash in
  Cmd.git ~cwd:t.repo ["branch"; "-f"; branch; hash]

let marshal t = to_yojson t |> Yojson.Safe.to_string
let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok x -> x
  | Error e -> failwith e
