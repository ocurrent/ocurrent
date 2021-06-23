open Current.Syntax
open Lwt.Infix

let src = Logs.Src.create "current.fs" ~doc:"OCurrent filesystem plugin"

module Log = (val Logs.src_log src : Logs.LOG)

let save path value =
  Current.component "save"
  |> let> path = path and> value = value in
     Current_incr.const
       (match Bos.OS.File.read path with
       | Ok old when old = value ->
           Log.info (fun f -> f "No change for %a" Fpath.pp path);
           (Ok (), None)
       | Error _ as e when Bos.OS.File.exists path = Ok true -> (e, None)
       | _ -> (
           (* Old contents differ, or file doesn't exist. *)
           match Bos.OS.File.write path value with
           | Ok () ->
               Log.info (fun f -> f "Updated %a" Fpath.pp path);
               (Ok (), None)
           | Error _ as e -> (e, None)))

let next_id =
  let i = ref 0 in
  fun () ->
    let id = !i in
    incr i;
    id

let make_monitor f path =
  let read () = f path in
  Logs.info (fun f -> f "Installing monitor");
  let watch refresh =
    Log.debug (fun f -> f "Installing watch for %a" Fpath.pp path);
    Irmin_watcher.hook (next_id ()) (Fpath.to_string path) (fun cpath ->
        Log.info (fun f -> f "Detected change in %S" cpath);
        refresh ();
        Lwt.return_unit)
    >|= fun unwatch ->
    Log.debug (fun f -> f "Watch installed for %a" Fpath.pp path);
    fun () ->
      Log.debug (fun f -> f "Unwatching %a" Fpath.pp path);
      unwatch ()
  in
  let pp f = Fpath.pp f path in
  Current.Monitor.create ~read ~watch ~pp

module Write = struct
  type t = Fpath.t

  module Key = struct 
    type t = string

    let digest  = Digest.string
  end

  module Value = Current.Unit
  module File_map = Map.Make (Fpath)

  let file_locks = ref File_map.empty

  let file_lock file =
    match File_map.find_opt file !file_locks with
    | Some l -> l
    | None ->
        let l = Lwt_mutex.create () in
        file_locks := File_map.add file l !file_locks;
        l

  let id = "fs-write"

  let pp ppf s = Fmt.pf ppf "fs write: %s" s

  let build file job write =
    Lwt_mutex.with_lock (file_lock file) @@ fun () ->
    Current.Job.start job ~level:Mostly_harmless >|= fun () ->
    Bos.OS.File.write file write

  let auto_cancel = true
end

module WC = Current_cache.Make (Write)

module File = struct
  let read path =
    Current.component "read %a" Fpath.pp path
  |> let> () = Current.return () in
      Current.Monitor.get @@ make_monitor (fun t -> Lwt.return (Bos.OS.File.read t)) path

  let write path write =
    Current.component "write"
    |> let> write = write in 
       WC.get path write
end

module Dir = struct
  let contents ?(recursive = false) path = 
    let f t = 
      if recursive then Lwt.return @@ Bos.OS.Dir.fold_contents (fun p acc -> p :: acc) [] t 
      else Lwt.return @@ Bos.OS.Dir.contents t 
    in 
    Current.component "read %a" Fpath.pp path
  |> let> () = Current.return () in
      Current.Monitor.get @@ make_monitor f path
end