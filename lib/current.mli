module Input : sig
  type t = string
end

include Current_term.S.TERM with type input := Input.t

module Analysis : Current_term.S.ANALYSIS

module Executor : Current_term.S.EXECUTOR with
  type 'a term := 'a t and
  type analysis := Analysis.t and
  type input := Input.t
