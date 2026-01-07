(lang dune 3.3)
(name current)

(generate_opam_files true)
(formatting(enabled_for dune))
(using mdx 0.3)

(source (github ocurrent/ocurrent))
(maintainers "Mark Elvers <mark.elvers@tunbury.org>")
(authors
  "Thomas Leonard <talex5@gmail.com>"
  "Antonin Décimo <antonin@tarides.com>"
  "Tim McGilchrist <timmcgil@gmail.com>"
  "Craig Ferguson <me@craigfe.io>"
  "Etienne MARAIS <etienne@maiste.fr>"
  "Anil Madhavapeddy <anil@recoil.org>"
  "David Allsopp <david.allsopp@metastack.com>"
  "Ewan Mellor <ewan@tarides.com>"
  "Kate <kit.ty.kate@disroot.org>"
  "Mark Elvers <mark.elvers@tunbury.org>"
  "Puneeth Chaganti <punchagan@muse-amuse.in>"
  "Lucas Pluvinage <lucas@tarides.com>"
  "Navin Keswani <navin@novemberkilo.com>"
  "Thomas Gazagnaire <thomas@gazagnaire.org>"
  "Patrick Ferris <patrick@sirref.org>"
  "Arthur Wendling <art.wendling@gmail.com>"
  "Anurag Soni <anurag@sonianurag.com>"
  "Ambre Austen Suhamy <ambre@tarides.com>"
  "Ben Andrew <benmandrew@gmail.com>"
  "Gargi Sharma <gs051095@gmail.com>"
  "Jonathan Coates <git@squiddev.cc>"
  "Jules Aguillon <juloo.dsi@gmail.com>"
  "Magnus Skjegstad <magnus@skjegstad.com>"
  "Shon Feder <shon.feder@gmail.com>"
  "smolck <46855713+smolck@users.noreply.github.com>"
  "tatchi <corentin.leruth@gmail.com>")
(documentation "https://ocurrent.github.io/ocurrent/")
(license "Apache-2.0")

(package
 (name current)
 (synopsis "Pipeline language for keeping things up-to-date")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

It is used in ocaml-ci (which provides CI for OCaml projects on GitHub),
and in docker-base-images (a pipeline that builds Docker images for various
Linux distributions, OCaml compiler versions and CPU types, and pushes them
to Docker Hub).

A pipeline is written much like you would write a one-shot sequential script,
but OCurrent will automatically re-run steps when the inputs change, and will
run steps in parallel where possible.")
 (depends
  (ocaml (>= 4.12.0))
  (astring (>= 0.8.5))
  bos
  (cmdliner (>= 1.1.0))
  (conf-libev (<> :os win32))
  (current_incr (>= 0.6.1))
  duration
  (fmt (>= 0.8.9))
  (fpath (>= 0.7.3))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  lwt-dllist
  ppx_deriving
  prometheus
  (re (>= 1.9.0))
  (result (>= 1.5))
  sqlite3
  (alcotest (and :with-test (>= 1.2.0)))
  (alcotest-lwt (and :with-test (>= 1.2.0)))
  (prometheus-app (and :with-test (>= 1.2)))))

(package
 (name current_git)
 (synopsis "Git plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides primitives for interacting with Git.
It can pull from remote repositories, or monitor local ones for changes.")
 (depends
  (current (= :version))
  (ocaml (>= 4.12.0))
  (astring (>= 0.8.5))
  (base64 (>= 3.1.0))
  (bos (>= 0.2.0))
  conf-git
  (cstruct (>= 6.0.0))
  (fmt (>= 0.8.9))
  (fpath (>= 0.7.3))
  irmin-watcher
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (mirage-crypto (and (>= 0.8.0) (< 1.0.0)))
  ppx_deriving
  (ppx_deriving_yojson (>= 3.5.1))
  (result (>= 1.5))
  yojson
  (mdx :with-test)
  (alcotest (and :with-test (>= 1.2.0)))
  (alcotest-lwt (and :with-test (>= 1.2.0)))))

(package
 (name current_web)
 (synopsis "Test web UI for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides a basic web UI for service administrators.
It shows the current pipeline visually and allows viewing job
logs and configuring the log analyser.")
 (depends
  (crunch (and :build (>= 3.3.0)))
  (current (= :version))
  (ocaml (>= 4.12.0))
  (ansi (>= 0.5.0))
  (astring (>= 0.8.5))
  base64
  bos
  (cmdliner (>= 1.1.0))
  (cohttp-lwt-unix (>= 6.0.0))
  (conduit-lwt-unix (>= 2.2.2))
  conf-graphviz
  (cstruct (>= 5.2.0))
  (csv (>= 2.4))
  (fmt (>= 0.8.9))
  (fpath (>= 0.7.3))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (mirage-crypto (and (>= 0.8.7) (< 1.0.0)))
  (mirage-crypto-rng (and (>= 0.11.0) (< 1.0.0)))
  (mirage-crypto-rng-lwt (and (>= 0.11.0) (< 1.0.0)))
  (multipart_form-lwt (>= 0.4.0))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.5.1))
  (ppx_sexp_conv (>= v0.14.1))
  (prometheus (>= 0.7))
  (prometheus-app (>= 1.2))
  (re (>= 1.9.0))
  (result (>= 1.5))
  (routes (>= 2.0.0))
  session
  session-cohttp-lwt
  (sexplib (>= v0.14.0))
  (sqlite3 (>= 5.0.2))
  (tyxml (>= 4.6.0))
  (uri (>= 4.0.0))
  (yojson (>= 1.7.0))))

(package
 (name current_docker)
 (synopsis "OCurrent Docker plugin")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides a plugin for interacting with Docker.
It can pull, build, run and push images, and can coordinate
multiple Docker Engine instances.")
 (depends
  (current (= :version))
  (current_git (= :version))
  (ocaml (>= 4.12.0))
  (astring (>= 0.8.5))
  (bos (>= 0.2.0))
  (duration (>= 0.1.3))
  (fmt (>= 0.8.9))
  (fpath (>= 0.7.3))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  ppx_deriving
  (ppx_deriving_yojson (>= 3.5.1))
  (result (>= 1.5))
  yojson))

(package
 (name current_github)
 (synopsis "GitHub plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides primitives for interacting with GitHub.
It can monitor and clone remote GitHub repositories, and can
push GitHub status messages to show the results of testing
PRs and branches.")
 (depends
  (current (= :version))
  (current_git (= :version))
  (current_web (= :version))
  (ocaml (>= 4.12.0))
  (astring (>= 0.8.5))
  (base64 (>= 3.4.0))
  (cmdliner (>= 1.1.0))
  (cohttp-lwt-unix (>= 6.0.0))
  (cstruct (>= 5.2.0))
  duration
  (fmt (>= 0.8.9))
  (github-unix (>= 4.4.0))
  (hex (>= 1.4.0))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (mirage-crypto (< 1.0.0))
  (mirage-crypto-pk (< 1.0.0))
  (ppx_deriving_yojson (>= 3.6.1))
  (prometheus (>= 0.7))
  ptime
  (result (>= 1.5))
  (rresult (>= 0.6.0))
  (tls-lwt (>= 0.16.0))
  (tyxml (>= 4.6.0))
  (uri (>= 4.0.0))
  (x509 (>= 0.10.0))
  yojson))

(package
 (name current_gitlab)
 (synopsis "GitLab plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides primitives for interacting with GitLab.
It can monitor and clone remote GitLab repositories, and can
push GitLab status messages to show the results of testing
PRs and branches.")
 (authors "Tim McGilchrist <timmcgil@gmail.com>")
 (maintainers "Tim McGilchrist <timmcgil@gmail.com>")
 (depends
  (current (= :version))
  (current_git (= :version))
  (current_web (= :version))
  (ocaml (>= 4.12.0))
  (cmdliner (>= 1.1.0))
  (cohttp-lwt-unix (>= 6.0.0))
  (fmt (>= 0.8.9))
  (gitlab-unix (>= 0.1.8))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (ppx_deriving_yojson (>= 3.6.1))
  (prometheus (>= 0.7))
  ptime
  (result (>= 1.5))
  (rresult (>= 0.6.0))
  yojson))

(package
 (name current_rpc)
 (synopsis "Cap'n Proto RPC plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides a Cap'n Proto RPC interface, allowing
an OCurrent engine to be controlled remotely.")
 (depends
  (ocaml (>= 4.12.0))
  (capnp (>= 3.4.0))
  (capnp-rpc (>= 1.2.3))
  (capnp-rpc-lwt (>= 1.2.3))
  (fmt (>= 0.8.9))
  fpath
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (result (>= 1.5))
  (stdint (>= 0.7.0)))
 (conflicts
  (x509 (= 0.11.0))))

(package
 (name current_slack)
 (synopsis "Slack plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides primitives for interacting with Slack.
It can post messages to slack channels.")
 (depends
  (current (= :version))
  (ocaml (>= 4.12.0))
  (cohttp-lwt-unix (>= 6.0.0))
  (fmt (>= 0.8.9))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (tls-lwt (>= 0.16.0))
  (uri (>= 4.0.0))
  yojson))

(package
 (name current_ssh)
 (synopsis "SSH plugin for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides a plugin for running ssh commands.")
 (authors "Mark Elvers <mark.elvers@tunbury.org>")
 (maintainers "Mark Elvers <mark.elvers@tunbury.org>")
 (depends
  (current (= :version))
  (ocaml (>= 4.12.0))
  yojson))

(package
 (name current_examples)
 (synopsis "Example pipelines for OCurrent")
 (description "\
OCurrent provides an OCaml eDSL for writing CI/CD pipelines.

This package provides some example pipelines.
It exists mainly to test the integration of various OCurrent
plugins.")
 (depends
  (current (= :version))
  (current_docker (= :version))
  (current_git (= :version))
  (current_github (= :version))
  (current_gitlab (= :version))
  (current_rpc (= :version))
  (current_web (= :version))
  (current_ssh (= :version))
  (ocaml (>= 4.12.0))
  (capnp-rpc (>= 1.2.3))
  (capnp-rpc-lwt (>= 1.2.3))
  (capnp-rpc-net (>= 1.2.3))
  (capnp-rpc-unix (>= 1.2.3))
  (cmdliner (>= 1.1.0))
  duration
  (dockerfile (>= 7.0.0))
  (fmt (>= 0.8.9))
  (fpath (>= 0.7.3))
  (logs (>= 0.7.0))
  (lwt (>= 5.7))
  (ppx_deriving (>= 5.1))
  (ppx_deriving_yojson (>= 3.6.1))
  (prometheus (>= 0.7))
  (prometheus-app (>= 1.2))
  (result (>= 1.5))
  (routes (>= 2.0.0))
  (uri (>= 4.0.0))
  (yojson (>= 1.7.0))
  (mdx :with-test)))
