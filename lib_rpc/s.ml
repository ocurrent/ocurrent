(** The subset of the Current API that the RPC system needs.
    This is duplicated here to avoid making RPC clients depend on
    the "current" service implementation package. *)

module type CURRENT = sig
  class type actions = object
    method pp : Format.formatter -> unit
    method rebuild : (unit -> string) option
  end

  module Job : sig
    type t
    module Map : Map.S with type key = string
    val log_path : string -> (Fpath.t, [`Msg of string]) result
    val lookup_running : string -> t option
    val wait_for_log_data : t -> unit Lwt.t
    val approve_early_start : t -> unit
    val cancel : t -> string -> unit
    val cancelled_state : t -> (unit, [`Msg of string]) result
  end

  module Engine : sig
    type t
    type results

    val state : t -> results
    val jobs : results -> actions Job.Map.t
  end
end
