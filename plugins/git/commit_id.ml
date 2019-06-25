[@@@ocaml.warning "-3"]

type t = {
  repo : string;  (* Remote repository from which to pull. *)
  gref : string;  (* Ref to pull, e.g. "master" or "pull/12/head". *)
  hash : string;  (* Hash that [gref] is expected to have. *)
} [@@deriving yojson]

[@@@ocaml.warning "+3"]

let ensure_no_spaces x =
  if String.contains x ' ' then
    Fmt.failwith "Spaces are not allowed here (in %S)" x

let v ~repo ~gref ~hash =
  ensure_no_spaces repo;
  ensure_no_spaces gref;
  ensure_no_spaces hash;
  { repo; gref; hash }

let pp f {repo; gref; hash} =
  Fmt.pf f "%s#%s (%s)" repo gref hash

let is_local t =
  if Astring.String.is_prefix ~affix:"file:" t.repo then true
  else match String.index_opt t.repo ':',
             String.index_opt t.repo '/' with
  | Some i, Some j -> i < j     (* http://... is remote; /http:foo is local *)
  | None, _ -> true             (* All remote URLs have colons *)
  | Some _, None -> false       (* foo:bar is remote *)

let equal = (=)
let compare = compare
let digest {repo; gref; hash} = Fmt.strf "%s %s %s" repo gref hash
