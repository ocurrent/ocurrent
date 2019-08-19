FROM ocaml/opam2:4.08
RUN sudo apt-get update && sudo apt-get install graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN git pull origin master && git reset --hard b70af589a3a2e7aa2202b499b8c669fa4e51bf42 && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
