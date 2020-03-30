(* Static analysis of a build pipeline. *)

module Make (Metadata : sig type t end) : sig
  type 'a t

  val stats : _ t -> S.stats

  val pp : _ t Fmt.t
  val pp_dot : env:(string * string) list -> url:(Metadata.t S.link -> string option) -> _ t Fmt.t
end with type 'a t := 'a Node.Make(Metadata).t
