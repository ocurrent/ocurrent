(** Health check tools. *)

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

val dig : schedule:Current_cache.Schedule.t -> fqdn:string -> Dig.Value.t Current.t
(** [dig ~schedule ~fqdn] resolves FQDN into A and AAAA records. *)

val curl : schedule:Current_cache.Schedule.t -> address:(Address.t Current.t) -> unit Current.t
(** [curl ~schedule ~address] Attempts to download from the given FQDN/IP using HTTPS. *)

val ping : schedule:Current_cache.Schedule.t -> address:(Address.t Current.t) -> unit Current.t
(** [ping ~schedule ~address] pings the given IP address. *)

