FROM ocaml/opam2:4.07
RUN sudo apt-get update && sudo apt-get install graphviz m4 -y --no-install-recommends
ADD --chown=opam *.opam /src/
RUN opam install -y --deps-only -t /src
ADD --chown=opam . /src
RUN opam config exec -- make -C /src
