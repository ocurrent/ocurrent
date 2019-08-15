FROM ocaml/opam2:4.08
RUN sudo apt-get update && sudo apt-get install graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN git pull origin master && git reset --hard f372039db86a970ef3e662adbfe0d4f5cd980701 && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
