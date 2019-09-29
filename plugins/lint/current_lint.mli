module Docker = Current_docker.Default

val v_from_opam :
  ?ocamlformat_version:string ->
  base:Docker.Image.t Current.t ->
  src:Current_git.Commit.t Current.t ->
  unit Current.t
(** [v ~base ~src] runs a Dune linting check on [src] via the base image [base]
    with OPAM installed. If [ocamlformat_version] is not passed, it will be
    inferred from the [.ocamlformat] file in the root of [src]. *)
