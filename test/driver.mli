val init_logging : unit -> unit

val test :
  ?config:Current.Config.t ->
  ?final_stats:Current_term.S.stats ->
  name:string ->
  (unit -> unit Current.t) ->
  (int -> unit) ->
  unit
(** [test ~name pipeline actions] runs [pipeline]. After each iteration,
    it calls [actions i] where [i] is the number of the next step ([1] on the
    first call). If [actions i] raises [Exit] then the tests finish. *)

val cancel : string -> unit
(** [cancel msg] cancels the job named [msg]. *)

val rebuild : string -> unit
(** [rebuild msg] triggers a rebuild of the job named [msg]. *)

val test_case_gc : string -> (Eio.Switch.t -> unit -> unit) -> unit Alcotest.test_case
