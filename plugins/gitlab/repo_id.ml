type t = {
  owner : string;
  name : string;
  project_id: int;
} [@@deriving to_yojson]

let pp f { owner; name; project_id } = Fmt.pf f "%s/%s/%i" owner name project_id

let to_git f { owner; name; _ } = Fmt.pf f "%s/%s" owner name

let compare = compare

let cmdliner =
  let open Cmdliner in
  let parse s =
    match Astring.String.cuts ~sep:"/" s with
    | [ owner; name; project_id ] -> Ok { owner; name; project_id=int_of_string project_id }
    | _ -> Fmt.error_msg "%S not in the form 'owner/name/project_id'" s
  in
  Arg.(
    required &
    pos 0 (some (conv ~docv:"REPO" (parse, pp))) None &
    info
      ~doc:"The GitLab repository (owner/name/project_id) to monitor."
      ~docv:"REPO"
      [])
