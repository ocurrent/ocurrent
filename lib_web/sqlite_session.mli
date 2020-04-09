(** An ocaml-session backend for sqlite *)

include Session.S.Now with
  type key = string and
  type value = string and
  type period = Int64.t

val create : Current.Db.t -> t
