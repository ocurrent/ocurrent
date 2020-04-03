### v0.2

The main new feature is that OCurrent now evaluates pipelines incrementally.
This means that services with large pipelines (such as ocaml-ci, with around
10,000 stages) can decide what they need to do without using an excessive
amount of CPU time.

- Replace changed promises with `Engine.update` (@talex5, #135)
- Evaluate pipelines incrementally (@talex5, #144)
- Avoid unnecessary re-evaluations in `Current.list_map` (@talex5, #148)
- Change the representation of terms to support incremental evaluation (@talex5, #150)

Documentation:

- Added [Internals](https://github.com/ocurrent/ocurrent/wiki/Internals) wiki
  page explaining how OCurrent works.
- Update README.md to link the [API Docs](https://ocurrent.github.io/ocurrent/index.html) (@shonfeder, #145)
- Fix typo in README ("about" -> "above") (@smolck, #100)

Docker plugin:

- Add support for run-arguments in docker plugin (@MagnusS, #96)
- Add `?build_args` parameter to `Current_docker` build (@SquidDev, #102)
- Allow alternative Dockerfile filenames in `Docker.build` (@talex5, #111)
- Add `Docker.pread` to get container stdout as a string (@kit-ty-kate, #120)
- Make it easier to add custom Docker commands (@talex5, #130)

Git plugin:

- Git.fetch: don't take repository lock until the job starts (@talex5, #116)

GitHub plugin:

- Add `Github.Api.head_of` to track individual branches (@talex5, #114)
- Expose the low-level `refs` function to get access to branch names and PR
  numbers (@talex5, #123)
- Add `Commit.uri` to make links back to GitHub (@talex5, #131)
- Better error if setting a GitHub commit status fails (@talex5, #151)
- Only refresh the repository that generated the webhook event, not all of them (@talex5, #156)

Web UI:

- Use routes library to simplify routing (@talex5, #160)
- Add "/jobs" page with information about active jobs (@talex5, #125)
- Mark HTML pages as UTF-8 (@talex5, #128)

Dependencies:

- Update to new Alcotest API (@talex5, #121)
- Use mirage-crypto instead of nocrypto (@hannesm, #161)

Bug fixes:

- Fix analysis with hidden `catch`/`state` nodes (@talex5, #134)
- Allow cancelling a job while waiting for confirmation (@talex5 #137)
- Fix merging of error states for pairs (@talex5, #147)

Metrics:

- Report Prometheus metrics for pipeline stage states (@talex5, #115)
- Report metric for cache evaluations (@talex5, #143)

Build improvements:

- Make dune transitive dependencies explicit (@CraigFe, #97)
- Add OCaml-CI status badge to the README (@CraigFe, #107)

Other:

- Explicitly enumerate accepted `--confirm` values (@CraigFe, #108)
- Rename `Input` to `Primitive` and simplify the API (@talex5, #154)
- Call `Lwt_main.yield` while testing log patterns (@talex5, #155)
- Add `Current.collapse` to allow collapsing parts of diagrams (@talex5, #152)
- Add `Current.with_context` to help with diagram layout (@talex5, #159)

### v0.1

Initial release.
