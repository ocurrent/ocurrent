OCurrent allows you to specify a workflow / pipeline for keeping things up-to-date.

<p align='center'>
  <img src="./doc/gated-deploy.svg"/>
</p>

Status: **experimental / prototyping**

For example, the pipeline shown about fetches the head of a GitHub repository's
`master` branch, builds it, runs the tests, and deploys the binary if the tests
pass. When a new commit is pushed, it runs the pipeline again.

Another use might be to keep the GitHub build status of each PR in your Git
repository showing the result of fetching, building and testing the PR's head
commit. If the head commit changes, the result must be recalculated.

An OCurrent pipeline is written using an OCaml eDSL. When OCurrent evaluates it,
it records the inputs used (e.g. the current set of open PRs and the head of each
one), monitors them, and automatically recalculates when an input changes.

[docker_build_local.ml][] contains a simple example pipeline:

```ocaml
(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let head = Git.Local.(commit_of_ref repo (head repo)) in
  let src = Git.fetch head in
  let+ _image = Docker.build src in
  ()
```

This monitors a local Git repository (`repo`), from which it gets the current branch
(`(head repo)`, from `$repo/.git/HEAD`) and then the head commit on that branch
(`commit_of_ref (head repo)`). It copies it to a temporary clone (`Git.fetch`) and builds
it with `docker build /path/to/clone`.

You can test it using a clone of the OCurrent repository itself:

```bash
$ git clone https://github.com/talex5/ocurrent.git
$ cd ocurrent/
$ dune exec -- ./examples/docker_build_local.exe .
[...]
    current [INFO] Evaluation complete:
                     Result: Pending
                     Watching: [/home/user/ocurrent/#refs/heads/master;
                                HEAD(/home/user/ocurrent/)]
[...]
current.docker [INFO] Build of docker image "build-of-d75e33fd875d80cd8e0cddf83904dd6d7aea12d3" succeeded
    current [INFO] Evaluation complete:
                     Result: Ok ()
                     Watching: [/home/user/ocurrent/#refs/heads/master;
                                HEAD(/home/user/ocurrent/)]
```

If you make a new commit or change branch (e.g. `git checkout -b test HEAD~1`) then OCurrent will
notice and build it again.

OCurrent can generate a graph showing the current state of the pipeline.
The example code wraps the previous `pipeline` like this:

```ocaml
let dotfile = Fpath.v "pipeline.dot"

(* Render pipeline as dot file *)
let pipeline ~repo () =
  let result = pipeline ~repo () in
  let dot_data =
    let+ a = Current.Analysis.get result in
    Fmt.strf "%a" Current.Analysis.pp_dot a
  in
  let* () = Current_fs.save (Current.return dotfile) dot_data in
  result
```

This causes it to maintain also a `pipeline.dot` file showing the current state of the pipeline.
You can turn this into e.g. SVG with `dot -Tsvg pipeline.dot  -o pipeline.svg`:

<p align='center'>
  <img src="./doc/example1.svg"/>
</p>

A green box indicates a pipeline stage that succeeded, orange means
in-progress, grey means cannot be started yet, and red means failed.

OCurrent has a small core language (in `lib` and `lib_term`), but most
functionality is added by external libraries. See the [plugins][] directory for
some examples.

## TODO

OCurrent is very incomplete at the moment (but is being actively developed, as of June 2019).
Planned/missing features include:

- Integration with GitHub, not just local repositories.
- Caching build results to disk, not just in memory.
- A web-based UI.
- Cap'n Proto RPC, to allow remote control.

[docker_build_local.ml]: https://github.com/talex5/ocurrent/blob/master/examples/docker_build_local.ml
[plugins]: https://github.com/talex5/ocurrent/blob/master/plugins
