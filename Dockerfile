FROM ocaml/opam2:4.07
RUN sudo apt-get update && sudo apt-get install graphviz m4 pkg-config libsqlite3-dev -y --no-install-recommends
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
