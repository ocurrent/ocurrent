(* Some dummy build primitives. *)

type commit = private string
type source = private string
type binary = private string

val fetch : commit OCurrent.t -> source OCurrent.t
(** [fetch commit] pulls [commit] to a local Git repository. *)

val build : ?on:string -> source OCurrent.t -> binary OCurrent.t
(** [build ~on:platform src] builds a binary from source code. *)

val test : ?src:source OCurrent.t -> binary OCurrent.t -> unit OCurrent.t
(** [test ~src bin] runs the unit-tests in [src] on the binary [bin]. *)

val deploy : binary OCurrent.t -> unit OCurrent.t
(** [deploy x] deploys/publishes [x]. *)

val revdeps : source OCurrent.t -> commit list OCurrent.t
(** [revdeps src] queries opam to discover all the packages depending on
    [src]. *)

val test_commit : commit
(** A dummy commit which can be used for testing. *)
