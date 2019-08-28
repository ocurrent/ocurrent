type t = {
  owner : string;
  name : string;
}

let pp f { owner; name } = Fmt.pf f "%s/%s" owner name

let compare = compare

let cmdliner =
  let open Cmdliner in
  let parse s =
    match Astring.String.cuts ~sep:"/" s with
    | [ owner; name] -> Ok { owner; name }
    | _ -> Error (`Msg (Fmt.strf "%S not in the form 'owner/name'" s))
  in
  Arg.conv ~docv:"REPO" (parse, pp)
