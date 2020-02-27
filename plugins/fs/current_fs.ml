open Current.Syntax

let src = Logs.Src.create "current.fs" ~doc:"OCurrent filesystem plugin"
module Log = (val Logs.src_log src : Logs.LOG)

let save path value =
  Current.component "save" |>
  let> path = path
  and> value = value in
  Current.Input.of_fn @@ fun _env ->
  let actions = object
    method pp f = Fmt.pf f "Save %a" Fpath.pp path
    method cancel = None
    method rebuild = None
    method release = ()
  end in
  match Bos.OS.File.read path with
  | Ok old when old = value ->
    Log.info (fun f -> f "No change for %a" Fpath.pp path);
    Ok (), Current.Input.metadata actions
  | Error _ as e when Bos.OS.File.exists path = Ok true ->
    e, Current.Input.metadata actions
  | _ ->
    (* Old contents differ, or file doesn't exist. *)
    match Bos.OS.File.write path value with
    | Ok () ->
      Log.info (fun f -> f "Updated %a" Fpath.pp path);
      Ok (), Current.Input.metadata actions
    | Error _ as e ->
      e, Current.Input.metadata actions

let read path =
  Current.component "read" |>
  let> path = path in
  Current.Input.of_fn @@ fun _env ->
  let actions = object
    method pp f = Fmt.pf f "Save %a" Fpath.pp path
    method cancel = None
    method rebuild = None
    method release = ()
  end in
  match Bos.OS.File.read path with
  | Ok value ->
    Ok (value), Current.Input.metadata actions
  | Error _ as e ->
    e, Current.Input.metadata actions
