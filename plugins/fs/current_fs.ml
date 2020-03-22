open Current.Syntax

let src = Logs.Src.create "current.fs" ~doc:"OCurrent filesystem plugin"
module Log = (val Logs.src_log src : Logs.LOG)

let save path value =
  Current.component "save" |>
  let> path = path
  and> value = value in
  Current_incr.const (
    match Bos.OS.File.read path with
    | Ok old when old = value ->
      Log.info (fun f -> f "No change for %a" Fpath.pp path);
      Ok (), None
    | Error _ as e when Bos.OS.File.exists path = Ok true ->
      e, None
    | _ ->
      (* Old contents differ, or file doesn't exist. *)
      match Bos.OS.File.write path value with
      | Ok () ->
        Log.info (fun f -> f "Updated %a" Fpath.pp path);
          Ok (), None
        | Error _ as e ->
          e, None
  )
