OCurrent allows you to specify a workflow / pipeline for keeping things up-to-date.

<p align='center'>
  <img src="./doc/gated-deploy.svg"/>
</p>

Status: **experimental**

For example, the pipeline shown about fetches the head of a GitHub repository's
`master` branch, builds it, runs the tests, and deploys the binary if the tests
pass. When a new commit is pushed, it runs the pipeline again.

Another use might be to keep the GitHub build status of each PR in your Git
repository showing the result of fetching, building and testing the PR's head
commit. If the head commit changes, the result must be recalculated.

An OCurrent pipeline is written using an OCaml eDSL. When OCurrent evaluates it,
it records the inputs used (e.g. the current set of open PRs and the head of each
one), monitors them, and automatically recalculates when an input changes.

The [OCurrent wiki][] contains documentation and examples.
In particular, you might like to start by reading about the
[example pipelines][] or how to [write your own plugins][writing-plugins].

Larger uses of OCurrent include the
[OCaml Docker base image builder][docker-base-images] and the CI that tests
this repository itself.

# Licensing

OCurrent is licensed under the Apache License, Version 2.0.
See [LICENSE][] for the full license text.

[docker-base-images]: https://github.com/ocaml-ci/docker-base-images
[writing-plugins]: https://github.com/ocaml-ci/ocurrent/wiki/Writing-plugins
[example pipelines]: https://github.com/ocaml-ci/ocurrent/wiki/Example-pipelines
[OCurrent wiki]: https://github.com/ocaml-ci/ocurrent/wiki
[LICENSE]: ./LICENSE
