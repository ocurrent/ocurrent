(* The core term language. *)

module S = S

module Output = Output

module Make (Input : S.INPUT) : sig
  include S.TERM with type input := Input.t

  module Analysis : S.ANALYSIS with type 'a term := 'a t

  module Executor : S.EXECUTOR with
    type 'a term := 'a t and
    type input := Input.t
end
