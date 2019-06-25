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

let id t = t.id.Commit_id.hash
let compare a b = String.compare (id a) (id b)
let equal a b = String.equal (id a) (id b)
let pp = Fmt.using id Fmt.string

let check_cached t =
  let hash = id t in
  let branch = Fmt.strf "fetch-%s" hash in
  Cmd.git ~cwd:t.repo ["branch"; "-f"; branch; hash]

let marshal t = to_yojson t |> Yojson.Safe.to_string
let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok x -> x
  | Error e -> failwith e
