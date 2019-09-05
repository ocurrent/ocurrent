FROM ocaml/opam2:4.08
RUN sudo apt-get update && sudo apt-get install graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN git pull origin master && git reset --hard 8bc187ff7168b47537d5bbd9b330a90ed90830ad && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
