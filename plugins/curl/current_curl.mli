(** Integration with Curl. *)

module Address : sig
  include Set.OrderedType

  val hash : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val pp_short : t Fmt.t
  val marshal : t -> string
  val unmarshal : string -> t
end

val expand : Dig.Value.t Current.t -> Address.t list Current.t

val resolve : schedule:Current_cache.Schedule.t -> fqdn:string -> Dig.Value.t Current.t
(** [clone ~schedule ~gref uri] evaluates to the head commit of [uri]'s [gref] branch (default: "master"). *)

val fetch : schedule:Current_cache.Schedule.t -> address:(Address.t Current.t) -> unit Current.t
(** [clone ~schedule ~gref uri] evaluates to the head commit of [uri]'s [gref] branch (default: "master"). *)

