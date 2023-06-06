
open Current.Syntax

(* This is the equivalent of the markdown file, but in the markdown file
   it is hard to have this globabl switch. *)

let _results, push_result =
  let s = Eio.Stream.create 64 in
  s, (fun v -> Eio.Stream.add s v)

module Show_files = struct
  type t = Eio.Fs.dir Eio.Path.t * Eio.Process.mgr

  let id = "show-files"

  module Key = struct
    include Current_git.Commit

    let digest t = Current_git.Commit_id.digest (id t)
  end

  module Value = Current.Unit

  let build (fs, proc) job commit =
    Current.Job.start job ~level:Current.Level.Harmless;
    Current_git.with_checkout ~fs ~job proc commit (fun tmpdir ->
      let files =
        Eio.Path.read_dir tmpdir
        |> List.filter (fun x -> x.[0] <> '.')
        |> List.sort String.compare
      in
      (* Fmt.pr "Building: %a@." Fmt.(Dump.list string) files; *)
      push_result (Some files);
      Ok ()
    )

  let pp = Current_git.Commit.pp
  let auto_cancel = false
end

module SF = Current_cache.Make(Show_files)

let show_files fs commit =
  Current.component "show_files" |>
  let> commit = commit in
  SF.get fs commit

let repo proc = Current_git.Local.v proc (Fpath.v "./main")

let pipeline ~ctx () =
    let remote_commit = Current_git.Local.head_commit (repo (snd ctx)) in
    let id = Current.map Current_git.Commit.id remote_commit in
    let clone = Current_git.fetch (snd ctx) id in
    let+ result = Current.catch (show_files ctx clone) in
    match result with
    | Ok () -> ()
    | Error (`Msg m) -> push_result (Some [m])

let engine ~ctx sw = Current.Engine.create ~sw (pipeline ~ctx)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.cwd env in
  let proc = (Eio.Stdenv.process_mgr env :> Eio.Process.mgr) in
  let ctx = fs, proc in
  Eio.Promise.await_exn @@ Current.Engine.thread (engine ~ctx sw)
