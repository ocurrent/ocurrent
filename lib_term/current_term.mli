(* The core term language. *)

module S = S

module Output = Output

module Make (Input : S.INPUT) : sig
  include S.TERM with type 'a input := 'a Input.t

  val env : Input.env t
  (** [env] evaluates to the user-provided environment. *)

  module Analysis : S.ANALYSIS with
    type 'a term := 'a t and
    type job_id := Input.job_id

  module Executor : S.EXECUTOR with
    type 'a term := 'a t and
    type env := Input.env and
    type analysis := Analysis.t
end
