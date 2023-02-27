open Lwt.Infix
open Current.Syntax

module Cmd = struct
  let exec_or_fail ?cwd ~name cmd =
    let cmd_s = String.concat " " cmd in
    let cmd = Array.of_list cmd in
    Lwt_process.exec ?cwd ("", cmd) >|= function
    | Unix.WEXITED n ->
        Alcotest.(check int) (Printf.sprintf "Process %s: %s" name cmd_s) 0 n
    | Unix.WSTOPPED _ -> Alcotest.fail "Process stopped."
    | Unix.WSIGNALED _ -> Alcotest.fail "Process received signal."

  let mkdir ?cwd dir =
    let cmd = [ "mkdir"; dir ] in
    exec_or_fail ?cwd ~name:"mkdir" cmd

  let rm ?cwd cmd =
    let cmd = "rm" :: cmd in
    exec_or_fail ?cwd ~name:"rm" cmd

  let mv ?cwd origin target =
    let cmd = [ "mv"; origin; target ] in
    exec_or_fail ?cwd ~name:"mv" cmd

  let touch ?cwd file =
    let cmd = [ "touch"; file ] in
    exec_or_fail ?cwd ~name:"touch" cmd

  let echo_to ?(cwd = "./") file content =
    let file = Filename.concat cwd file in
    Lwt_io.(
      with_file ~mode:Output file (fun cout -> Lwt_io.write_line cout content))

  let git ?cwd cmd =
    let cmd = "git" :: cmd in
    exec_or_fail ?cwd ~name:"git" cmd

  let git_with ?cwd ~path cmd =
    let cmd = "-C" :: path :: cmd in
    git ?cwd cmd
end

let results, push_result = Lwt_stream.create ()

module Show_files = struct
  type t = unit

  let id = "show-files"

  module Key = struct
    include Current_git.Commit

    let digest t = Current_git.Commit_id.digest (id t)
  end

  module Value = Current.Unit

  let build () job commit =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current_git.with_checkout ~job commit (fun tmpdir ->
        let files =
          Sys.readdir (Fpath.to_string tmpdir)
          |> Array.to_list
          |> List.filter (fun x -> x.[0] <> '.')
          |> List.sort String.compare
        in
        push_result (Some files);
        Lwt.return (Ok ()))

  let pp = Current_git.Commit.pp
  let auto_cancel = false
end

module SF = Current_cache.Make (Show_files)

let show_files commit =
  Current.component "show_files"
  |> let> commit = commit in
     SF.get () commit

let init root =
  let cwd = Fpath.to_string root in
  let dir = "sub" in
  Cmd.git ~cwd [ "config"; "--global"; "protocol.file.allow"; "always" ]
  >>= fun () ->
  Cmd.mkdir ~cwd dir >>= fun () ->
  Cmd.git ~cwd [ "init"; "-q"; dir ] >>= fun () ->
  let file = "sub/file" in
  Cmd.echo_to ~cwd file "sub" >>= fun () ->
  Cmd.git_with ~cwd ~path:"sub" [ "config"; "user.name"; "Name" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"sub" [ "config"; "user.email"; "test@example.com" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"sub" [ "add"; "file" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"sub"
    [ "commit"; "-q"; "-a"; "-m"; "Initial submodule commit" ]
  >>= fun () ->
  let dir = "main" in
  Cmd.mkdir ~cwd dir >>= fun () ->
  Cmd.git ~cwd [ "init"; "-q"; dir ] >>= fun () ->
  let file = "main/file" in
  Cmd.echo_to ~cwd file "main" >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "add"; "file" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "add"; "-q"; "../sub" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "config"; "user.name"; "Name" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "config"; "user.email"; "test@example.com" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Initial main commit" ]

let remove root =
  let cwd = Fpath.to_string root in
  Cmd.rm ~cwd [ "main/.gitmodules" ] >>= fun () ->
  Cmd.rm ~cwd [ "-r"; "main/sub" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Remove submodule" ]

let add_back cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.git_with ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; "../sub" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"main"
    [ "commit"; "-q"; "-a"; "-m"; "Restore submodule" ]

let update_submodules cwd =
  let cwd = Fpath.to_string cwd in
  Cmd.mv ~cwd "sub" "newsub" >>= fun () ->
  Cmd.echo_to ~cwd "newsub/file2" "sub2" >>= fun () ->
  Cmd.git_with ~cwd ~path:"newsub" [ "add"; "file2" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"newsub" [ "config"; "user.name"; "Name" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"newsub"
    [ "config"; "user.email"; "test@example.com" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"newsub" [ "commit"; "-q"; "-a"; "-m"; "sub2" ]

let move_submodule cwd_f =
  let cwd = Fpath.to_string cwd_f in
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "deinit"; "-q"; "--all" ]
  >>= fun () ->
  Cmd.rm ~cwd [ "main/.gitmodules" ] >>= fun () ->
  Cmd.touch ~cwd "main/.gitmodules" >>= fun () ->
  Cmd.rm ~cwd [ "-r"; "main/sub" ] >>= fun () ->
  let path = Fpath.(add_seg cwd_f "newsub" |> to_string) in
  Cmd.git_with ~cwd ~path:"main"
    [ "submodule"; "add"; "--force"; "-q"; path; "sub" ]
  >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "submodule"; "sync"; "-q" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"main/sub" [ "pull"; "-q"; "origin" ] >>= fun () ->
  Cmd.git_with ~cwd ~path:"main" [ "commit"; "-q"; "-a"; "-m"; "Move module" ]

let check_result label expected stream =
  Lwt_stream.get stream >|= fun value ->
  Alcotest.(check (option (list string)) label value expected)

let test_lwt _switch () =
  let test =
    Lwt_io.create_temp_dir ~prefix:"current-git" () >>= fun dir ->
    let dir = Fpath.v dir in
    init dir >>= fun () ->
    let repo = Current_git.Local.v (Fpath.add_seg dir "main") in
    let pipeline () =
      let remote_commit = Current_git.Local.head_commit repo in
      let id = Current.map Current_git.Commit.id remote_commit in
      let clone = Current_git.fetch id in
      let+ result = Current.catch (show_files clone) in
      match result with
      | Ok () -> ()
      | Error (`Msg m) -> push_result (Some [ m ])
    in
    let _engine = Current.Engine.create pipeline in
    let expected = Some [ "file"; "sub" ] in
    check_result "Initial state" expected results >>= fun () ->
    remove dir >>= fun () ->
    let expected = Some [ "file" ] in
    check_result "After remove" expected results >>= fun () ->
    add_back dir >>= fun () ->
    let expected = Some [ "file"; "sub" ] in
    check_result "After restore" expected results >>= fun () ->
    update_submodules dir >>= fun () ->
    move_submodule dir >>= fun () ->
    let expected = Some [ "file"; "sub" ] in
    check_result "Final state" expected results
  in
  let timeout = Lwt_unix.sleep 120. >|= fun () -> Alcotest.fail "Timeout" in
  Lwt.pick [ test; timeout ]

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "current-git"
       [ ("mdx-like", [ Alcotest_lwt.test_case "full test" `Quick test_lwt ]) ]
