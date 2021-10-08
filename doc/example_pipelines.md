### Example 1 : `docker_build_local.ml`

[docker_build_local.ml](examples/docker_build_local.ml) contains a simple example pipeline:

```ocaml file=examples/docker_build_local.ml,part=pipeline
(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let image = Docker.build ~pull ~timeout (`Git src) in
  Docker.run image ~args:["dune"; "exec"; "--"; "docker_build_local"; "--help"]
```

This monitors a local Git repository (`repo`), from which it gets the current head commit.
It copies it to a temporary clone and builds it with `docker build /path/to/clone`, then
executes the example with `--help` to check that it runs.

You can test it using a clone of the OCurrent repository itself:

```
$ dune exec -- examples/docker_build_local.exe .
   current_web [INFO] Starting web server: (TCP (Port 8080))
       current [INFO] Result: Running
[...]
current.docker [INFO] Built docker image sha256:caf89896b8ad0fe08dede715b3b0f73815b4c6e7687dc0b38cb4a269a5ce7106
[...]
       current [INFO] Result: Ok: ()
```

If you make a new commit or change branch (e.g. `git checkout -b test HEAD~1`) then OCurrent will
notice and build it again.

The example code above works mostly with values of type `'a Current.t`.
For example, `Docker.build` takes a source current and returns a Docker image current.
If you have a function that works on concrete values then you can use `Current.map`
(or the `let+` syntax) to make it work on currents instead.

You can also use `Current.bind` (or the `let*` syntax) if you can only decide
what the next part of the pipeline should be by looking at a concrete input.
However, using `bind` limits OCurrent's ability to analyse the pipeline,
because it must wait for the input to be ready before knowing what happens
next.
It is almost never necessary to use a bind, and many large pipelines (such as
ocaml-ci) don't use any.
If you feel you must use a bind, consider using `let**` instead, which at least
allows you to label the box in the diagram.

OCurrent has a small core language (in `lib` and `lib_term`), but most
functionality is added by external libraries. See the [plugins](https://github.com/ocurrent/ocurrent/blob/master/plugins) directory for
some examples.

The example also runs a minimal web UI on port 8080 (use `--port=...` to change it),
showing the state of the system. You will need to have [graphviz](https://graphviz.org/) installed in order
to see the diagrams.

![example1](example1.svg)

A green box indicates a pipeline stage that succeeded, orange means
in-progress, grey means cannot be started yet (inputs not ready),
yellow means queued or waiting for permission to start, and red means failed.

Clicking on a box shows the log for that operation (though not all operations
have logs; `head commit` doesn't, for example).

### Example 2 : `build_matrix.ml`

[build_matrix.ml](examples/build_matrix.ml) contains a slightly more advanced pipeline:

![example2](example2.svg)

```ocaml file=examples/build_matrix.ml,part=pipeline
let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

(* Run "docker build" on the latest commit in Git repository [repo]. *)
let pipeline ~repo () =
  let src = Git.Local.head_commit repo in
  let build ocaml_version =
    let base = Docker.pull ~schedule:weekly ("ocaml/opam:debian-ocaml-" ^ ocaml_version) in
    let dockerfile =
      let+ base = base in
      `Contents (dockerfile ~base ~ocaml_version)
    in
    Docker.build ~label:ocaml_version ~pull:false ~dockerfile (`Git src) |>
    Docker.tag ~tag:(Fmt.str "example-%s" ocaml_version)
  in
  Current.all [
    build "4.10";
    build "4.11"
  ]
```

The `Docker.pull` step shows the use of a *schedule*. In this case, we consider
a pulled image to be valid for one week; after that OCurrent will automatically
run the `docker pull` again to check for newer versions.

It uses `Current.all` to build against different versions of OCaml, generating
a suitable Dockerfile for each version (the `ocaml/opam2` image contains multiple
versions of the compiler and the Dockerfile just selects one of them).

The generated images are then tagged with the compiler version used to build them.
