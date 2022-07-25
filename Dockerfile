FROM ocaml/opam:debian-11-ocaml-4.14@sha256:226af9efb4d241aed995aa313b318ae2eb6f3d7249b7a6f74a6c770179cc88a3
RUN sudo apt-get update && sudo apt-get install libev-dev libffi-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch origin master && git reset --hard afb42c54f75f42b0f77a27f9e71ea7748108b8c0 && opam update
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
