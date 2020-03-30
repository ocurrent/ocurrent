(* The core term language. *)

module S = S

module Output = Output

module Make (Primitive : S.PRIMITIVE) : sig
  include S.TERM with type 'a primitive := 'a Primitive.t

  module Analysis : S.ANALYSIS with
    type 'a term := 'a t and
    type job_id := Primitive.job_id

  module Executor : S.EXECUTOR with
    type 'a term := 'a t
end
