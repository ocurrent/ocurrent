FROM ocaml/opam:debian-11-ocaml-4.14@sha256:698cd4a3d59912f89deb3c8cb05b9299a870b27086d212877f5f3c88f4021b74 AS build
RUN sudo apt-get update && sudo apt-get install libev-dev libffi-dev capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git fetch -q origin master && git reset --hard 57f1b681ce75766a17f15588c2088174edbb89c9 && opam update
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
