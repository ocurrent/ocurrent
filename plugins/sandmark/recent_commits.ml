open Lwt.Infix

let id = "recent-commits"

type t = No_context

module Key = struct
  type t = {
    head : Current_git.Commit.t;
    n : int;
  } [@@deriving to_yojson]

  let digest { head; n } =
    Yojson.Safe.to_string @@ `Assoc [
      "head", `String (Current_git.Commit.id head);
      "n", `Int n;
    ]

  let pp f { head; n } =
    Fmt.pf f "Last %d commits from %a" n Current_git.Commit.pp head
end

module Value = struct
  type t = string list

  let marshal t =
    String.concat "\n" t

  let unmarshal x =
    Astring.String.cuts ~sep:"\n" ~empty:false x
end

let cmd ~cwd n =
  ("", Array.of_list ["git"; "-C"; Fpath.to_string cwd; "log"; "--pretty=%H"; "-n"; string_of_int n])

let build No_context job { Key.head; n } =
  Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
  Current_git.with_checkout ~job head @@ fun dir ->
  Current.Process.check_output ~cancellable:true ~job (cmd ~cwd:dir n) >|= function
  | Ok output ->
    let digests = Value.unmarshal output in
    Current.Job.log job "@[<v2>Results:@,%a@]" Fmt.(list string) digests;
    Ok digests
  | Error _ as e -> e

let level _ _ = Current.Level.Harmless

let auto_cancel = false

let pp = Key.pp
