FROM ocurrent/opam:debian-10-ocaml-4.08
RUN sudo apt-get update && sudo apt-get install capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin master && git reset --hard e785ffef91af8db14d493cfacbd53d211b7535da && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
