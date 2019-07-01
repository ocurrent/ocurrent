type t = No_context

let id = "docker-run"

module Key = struct
  type t = Image.t * string list

  let pp_args = Fmt.(list ~sep:sp (quote string))
  let pp f (image, args) = Fmt.pf f "docker run @[%a %a@]" Image.pp image pp_args args
  let digest (image, args) =
    Fmt.strf "%S %a" (Image.tag image) pp_args args
end

module Value = Current.Unit

let build ~switch No_context job key =
  let (image, args) = key in
  let cmd = Array.of_list @@ ["docker"; "run"; "-i"; image] @ args in
  Current_cache.Process.exec ~switch ~job ("", cmd)

let pp = Key.pp

let auto_cancel = true

let level _ _ = Current.Level.Average
