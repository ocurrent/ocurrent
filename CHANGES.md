### dev

Core:

- Optimise the SQL query for the Query page, by adding an
  index on the finish time and doing a case-insensitive
  search (@talex5 #215).

Docker plugin:

- Add an architecture flag in the Docker builder to enable
  multiarch builds (including 32-bit) (@avsm #213).

GitHub plugin:

- Use the term `allowlist` for permitted usernames and
  make it case-insensitive (@avsm #210, @Julow #211).

Build fixes and cleanup:

- Adapt to the Alcotest 1.2 interface (@talex5 #212).
- Add missing dependency on `ppx_deriving_yojson` (@talex5 #209).
- Remove unused `Option.map` (@talex5 #216)

### v0.3

The main changes are:

- "latching" of a previous pipeline stage output while building the new one.
  For example, while doing `docker pull` to check for updates to an image
  it will continue using the old one until the check finishes.
  Jobs with latched results are scheduled at a lower priority than jobs without
  any output yet.

- Many improvements to the web UI, including access controls, streaming of
  logs, improved querying, and custom extra pages.

Core:

- Allow user-defined pools (@talex5, #206).
  Add `Pool.of_fn` so that users can provide their own pools. This allows
  waiting for resources from an external service before marking a job as
  started.

- Add simple job priorities (@talex5, #187).
  Pools now have high and low priority queues.
  The cache puts jobs on the low priority queue if they have a latched result available.

- Allow jobs to latch previous output while rebuilding (@talex5, #178).
  If a rebuild is triggered by the schedule, do the rebuild in the background
  without changing the output to pending. This allows polling for changes
  without disrupting the rest of the pipeline.
  Such jobs are displayed with a gradient background (from the pending
  colour on the left to the latched output's colour on the right).

- Add `Current_cache.Generic` to control latching behaviour (@talex5, #179).
  This provides the full API. `BUILDER` and `PUBLISHER` are now special cases of `GENERIC`.

- Also latch failed results when cache entries expire (@talex5, #202).

- Don't clear latched result when auto-cancelling (@talex5, #195).

- Fix rebuilding of stale cache entries (@talex5, #169).
  If a build was already invalid when we loaded it from the database, we invalidated the key but didn't trigger an update properly.

- Fix `Option.get` crash when shutting down (@talex5, #205).
  If an error occurs on startup and a cache entry has expired then
  we set config to None to exit the engine, and the timer thread tries to
  read the config and crashes, hiding the real error.

- Fix scheduled builds for very fast jobs (@talex5, #185).
  We cleared the expiry timer when the job reported an active state. However,
  if the job was very quick (no async operations) we wouldn't see this state.
  Then we'd think the old timer was still in place and would be good to handle
  the new deadline too. Reported by Kate.

- Move `Current_cache.output` to new `Instance` submodule (@talex5, #201).
  The name "output" came from before the build and publish caches were unified and was confusing.

- Handle uncaught exceptions in monitor read functions (@talex5, #191).

- Add missing `?cwd` to `Current.Process.exec` (@kit-ty-kate, #177).

- Delete directories with `Lwt` (@talex5, #192).
  Using `Unix.unlink` was causing high latencies in some cases.

Web UI:

- Improvements to query page (@talex5, #196).
  - Add toggles to query page to allow rebuilding multiple jobs at once.
  - Filter by job ID prefix in the query page. This makes it easy to restrict results to a particular day (or month, hour, etc).
  - Filter by operation type in query page.
  - Allow filtering by rebuild status in query page.
  - Show job queue time and run time in query page.

- Stream log files (@talex5, #165).
  If the job is still running, keep the connection open and stream the data as it arrives.

- Add `Site` and `Context` modules (@talex5, #170).

- Auto-generate links in nav-bar from routes (@talex5, #182).
  Expose routes in web API. This allows the user to provide whatever routes they like, or to use their own server.

- Update to new Routes API (@anuragsoni, #189 #190).

- Add a basic access control system (@talex5, #172).
  - `Site` now takes `authn` and `has_role` arguments.
  - `authn` (if given) is used to create a "Login" link in the navbar.
  - `has_role` is used to decide who can access which resource.
  - The GitHub plugin now provides an authentication backend that authenticates users with GitHub.

- Display the build history in the job page (@talex5, #166).
  Allows navigating to previous build results easily.

Diagram generation:

- Extend, rather than replace, the context in `with_context` (@talex5, #197).

- Truncate long tooltips in Graphviz diagrams. Graphviz rejects long messages with the error `longer than 16384?`.

Docker plugin:

- Return the RepoId after pushing a manifest (@talex5, #207).

- Add `Raw` module with low-level API (@talex5, #168).
  This is useful if you need to create your own custom components (for example,
  because you want to use a `Current.t` input to generate one of the fixed
  arguments such as `~run_args`). See `examples/docker_custom.ml` for an example.

Git plugin:

- Add `?pool` argument to `Git.with_checkout` (@talex5, #184).
  Git checkout operations can use a lot of CPU and IO, so allow users to supply a pool to limit the number of concurrent operations.

- Make `Commit_id.pp_user_clone` public (@talex5, #176).
  Also, `Commit.id` now returns a `Commit_id.t`. Use `Commit.hash` to get the hash.

- Fix clone instructions for GitHub PRs (@talex5, #164).
  GitHub only provides the `refs/pull/NNN/head` branches when fetching, not when cloning (with the default refspec).
  Detect this case and show an alternative command that the user can run to reproduce the operation.

GitHub plugin:

- Get all installation repositories, not just the first 30 (@talex5, #193).
  For now, as a precaution, ignore any installation that has configured all repositories to be tested (this is likely an accident).

- If listing repositories fails, retry after 30s (@talex5, #198).

- Add `Installation.repositories ?include_archived` option (@talex5, #203).
  Filtering archived repositories is now the default.
  There's not much point doing CI on them as you can't write the status result back.

- Add `GitHub.Installation.account` to get account name (@talex5, #204).

- Add `Api.Anonymous.head_of` (@talex5, #200).
  This allows monitoring the head of a public GitHub repository without needing
  an OAuth token. It's useful for the `ocaml-ci-local` command, allowing users
  to test the pipeline easily.

- Catch cohttp end-of-file exceptions (@talex5, #175).

- Remove unicorn from error string (@talex5, #183).
  When a GraphQL query returned a server error, we previously returned the body
  of the HTTP response as the error message. However, this includes a large
  picture of a unicorn, which is not suitable as an error string.

Slack plugin:

- Upgrade TLS library to cope with Slack's new TLS policy (@talex5, #174 #194).

Build:

- Update to dune 2 (@talex5, #171).

- Add missing alcotest dependency (@talex5, #163).


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
