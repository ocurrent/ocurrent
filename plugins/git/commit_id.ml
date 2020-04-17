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

let repo t = t.repo
let gref t = t.gref
let hash t = t.hash

(* git-clone doesn't like the "refs/heads" prefix. *)
let strip_heads gref =
  let prefix = "refs/heads/" in
  let open Astring in
  if String.is_prefix ~affix:prefix gref then
    String.with_index_range ~first:(String.length prefix) gref
  else
    gref

let pp_user_clone f id =
  let short_hash = Astring.String.with_range ~len:8 id.hash in
  if Astring.String.is_prefix ~affix:"refs/pull/" id.gref then (
    (* GitHub doesn't recognise pull requests in clones, but it does in fetches. *)
    Fmt.pf f "git clone --recursive %S && cd %S && git fetch origin %S && git reset --hard %s"
      id.repo
      (Filename.basename id.repo |> Filename.remove_extension)
      (strip_heads id.gref)
      short_hash
  ) else (
    Fmt.pf f "git clone --recursive %S -b %S && cd %S && git reset --hard %s"
      id.repo
      (strip_heads id.gref)
      (Filename.basename id.repo |> Filename.remove_extension)
      short_hash
  )
