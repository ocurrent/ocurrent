(* Some dummy build primitives. *)

type source = Fpath.t

val revdeps : source Current.t -> Current_git_test.Commit.t list Current.t
(** [revdeps src] queries opam to discover all the packages depending on
    [src]. *)
