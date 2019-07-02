open Lwt.Infix

type t = No_context

let id = "docker-tag"

module Key = Current.String
module Value = struct
  type t = Image.t * [`Local | `Push]

  let digest = function
    | i, `Local -> "local:" ^ Image.marshal i
    | i, `Push -> "push:" ^ Image.marshal i
end

let publish ~switch No_context job tag (image, repo) =
  Current_cache.Process.exec ~switch ~job ("", [| "docker"; "tag"; Image.hash image; tag |]) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    match repo with
    | `Local -> Lwt.return @@ Ok ()
    | `Push -> Current_cache.Process.exec ~switch ~job ("", [| "docker"; "push"; tag |])

let pp f = function
  | (tag, (image, `Local)) -> Fmt.pf f "docker tag %a %S" Image.pp image tag
  | (tag, (image, `Push)) -> Fmt.pf f "docker push %a %S" Image.pp image tag

let auto_cancel = false

let level No_context _tag = function
  | (_, `Local) -> Current.Level.Average
  | (_, `Push) -> Current.Level.Dangerous
