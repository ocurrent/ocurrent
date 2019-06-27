val init_logging : unit -> unit

val test :
  ?config:Current.Config.t ->
  name:string ->
  (unit -> unit Current.t) ->
  (int -> unit) ->
  unit Lwt.t
(** [test ~name pipeline actions] runs [pipeline]. After each interation,
    it calls [actions i] where [i] is the number of the next step ([1] on the
    first call). If [actions i] raises [Exit] then the tests finish. *)

val cancel : string -> unit
(** [cancel msg] cancels the watch named [msg]. *)

exception Expect_skip
(** The [actions] callback can raise this if it's OK if the next step's inputs
    are ready immediately. Needed for the case of cache invalidation. *)
