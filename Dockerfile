FROM ocaml/opam:debian-11-ocaml-4.14@sha256:5b9de826b22c77a0654519d0959536f93a6ffd7020712a8b1c3437445e031e04
RUN sudo apt-get update && sudo apt-get install libev-dev libffi-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && git reset --hard 97da9a1b68b824a65a09e5f7d071fcf2da35bd1b && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam pin add -yn current_web.dev "./" && \
    opam pin add -yn current_slack.dev "./" && \
    opam pin add -yn current_rpc.dev "./" && \
    opam pin add -yn current_github.dev "./" && \
    opam pin add -yn current_gitlab.dev "./" && \
    opam pin add -yn current_git.dev "./" && \
    opam pin add -yn current_examples.dev "./" && \
    opam pin add -yn current_docker.dev "./" && \
    opam pin add -yn current.dev "./"
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
