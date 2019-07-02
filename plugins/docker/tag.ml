type t = No_context

let id = "docker-tag"

module Key = Current.String
module Value = Image

let publish ~switch No_context job tag image =
  Current_cache.Process.exec ~switch ~job ("", [| "docker"; "tag"; Image.hash image; tag |])

let pp f (tag, image) =
  Fmt.pf f "docker tag %a %S" Image.pp image tag

let auto_cancel = false

let level _ _  = Current.Level.Average
