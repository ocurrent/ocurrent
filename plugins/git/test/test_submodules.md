It's very difficult to get Git to behave sensibly when using submodules.
If you find a problem, please add a test-case to this file along with the fix.

Set up the environment:

```ocaml
# #require "current_git";;
# #require "lwt.unix";;
# open Current.Syntax;;
# open Lwt.Infix;;
# Unix.putenv "EMAIL" "test@example.com";;
- : unit = ()
```

Create an "upstream" repository for the submodule:

```sh
$ git config --global protocol.file.allow always
$ git config --global commit.gpgsign false
$ mkdir sub
$ git init -q sub
$ echo sub > sub/file
$ git -C sub add file
$ git -C sub commit -q -a -m 'Initial submodule commit'
```

Create the main repository and add the submodule to it:

```sh
$ mkdir main
$ git init -q main
$ echo main > main/file
$ git -C main add file
$ git -C main submodule add -q ../sub
$ git -C main commit -q -a -m 'Initial main commit'
```

Make an OCurrent job that just lists the files in a commit
(sending the sorted list to `results` on each build):

```ocaml
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
      (* Fmt.pr "Building: %a@." Fmt.(Dump.list string) files; *)
      push_result (Some files);
      Lwt.return (Ok ())
    )

  let pp = Current_git.Commit.pp
  let auto_cancel = false
end

module SF = Current_cache.Make(Show_files)

let show_files commit =
  Current.component "show_files" |>
  let> commit = commit in
  SF.get () commit
```

Start an OCurrent pipeline monitoring the main repository.
We watch "main" directly just to get the commit ID, then clone it as if it were remote:

```ocaml
# let repo = Current_git.Local.v (Fpath.v "./main");;
val repo : Current_git.Local.t = <abstr>
# let pipeline () =
    let remote_commit = Current_git.Local.head_commit repo in
    let id = Current.map Current_git.Commit.id remote_commit in
    let clone = Current_git.fetch id in
    let+ result = Current.catch (show_files clone) in
    match result with
    | Ok () -> ()
    | Error (`Msg m) -> push_result (Some [m]);;
val pipeline : unit -> unit Current.term = <fun>
# let engine = Current.Engine.create pipeline;;
val engine : Current.Engine.t = <abstr>
```

In the initial state we should see the submodule:

```ocaml
# Lwt_stream.get results;;
- : string list option = Some ["file"; "sub"]
```

Remove the submodule:

```sh
$ rm main/.gitmodules
$ rm -r main/sub
$ git -C main commit -q -a -m 'Remove submodule'
```

Ensure that our new checkout doesn't have the deleted submodule:

```ocaml
# Lwt_stream.get results;;
- : string list option = Some ["file"]
```

<!-- Add it back in again: -->

<!-- ```sh -->
<!-- $ git -C main submodule add --force -q ../sub >/dev/null -->
<!-- $ git -C main commit -q -a -m 'Restore submodule' -->
<!-- ``` -->

<!-- Ensure we re-create it in our checkout: -->

<!-- ```ocaml -->
<!-- # Lwt_stream.get results;; -->
<!-- - : string list option = Some ["file"; "sub"] -->
<!-- ``` -->

<!-- Update the submodule upstream: -->

<!-- ```sh -->
<!-- $ mv sub newsub -->
<!-- $ echo sub2 > newsub/file2 -->
<!-- $ git -C newsub add file2 -->
<!-- $ git -C newsub commit -q -a -m 'sub2' -->
<!-- ``` -->

<!-- Moving the submodule to a new location: -->

<!-- ```sh -->
<!-- $ git -C main submodule deinit -q --all -->
<!-- $ rm main/.gitmodules; touch main/.gitmodules -->
<!-- $ rm -r main/sub -->
<!-- $ git -C main submodule add --force -q "${PWD}/newsub" sub >/dev/null -->
<!-- $ git -C main submodule sync -q -->
<!-- $ git -C main/sub pull -q origin -->
<!-- $ git -C main commit -q -a -m 'Move module' -->
<!-- ``` -->

<!-- Ensure we fetch from the new location: -->

<!-- ```ocaml -->
<!-- # Lwt_stream.get results;; -->
<!-- - : string list option = Some ["file"; "sub"] -->
<!-- ``` -->
