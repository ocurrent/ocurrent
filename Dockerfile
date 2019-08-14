#syntax=docker/dockerfile-upstream
# The above line allows the --mount option below, which makes incremental builds faster.
# If you want to use older versions of Docker, you can just remove the --mount argument.
FROM ocaml/opam2:4.08
RUN sudo apt-get update && sudo apt-get install graphviz m4 pkg-config libsqlite3-dev -y --no-install-recommends
RUN git pull origin master && git reset --hard f372039db86a970ef3e662adbfe0d4f5cd980701 && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN --mount=type=cache,id=dune,target=/src/_build,sharing=locked,uid=1000,gid=1000 \
    opam config exec -- dune build --display=short @all && \
    sudo install _build/default/examples/docker_build_local.exe /usr/local/bin/docker_build_local
