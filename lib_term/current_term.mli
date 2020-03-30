(* The core term language. *)

module S = S

module Output = Output

module Make (Metadata : sig type t end) : sig
  include S.TERM with
    type metadata := Metadata.t and
    type 'a primitive = ('a Output.t * Metadata.t option) Current_incr.t

  module Analysis : S.ANALYSIS with
    type 'a term := 'a t and
    type metadata := Metadata.t

  module Executor : S.EXECUTOR with
    type 'a term := 'a t
end
