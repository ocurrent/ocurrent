FROM ocurrent/opam:debian-10-ocaml-4.08
RUN sudo apt-get update && sudo apt-get install capnproto graphviz m4 pkg-config libsqlite3-dev libgmp-dev -y --no-install-recommends
RUN cd ~/opam-repository && git pull origin master && git reset --hard 8b9c3436650a5e217b4b8242ed0234fd9c0fa1e6 && opam update
ADD --chown=opam *.opam /src/
WORKDIR /src
RUN opam install -y --deps-only -t .
ADD --chown=opam . .
RUN opam config exec -- make -C .
