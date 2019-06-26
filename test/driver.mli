val init_logging : unit -> unit

val test_commit : Current_git_test.Commit.t

val test :
  ?config:Current.Config.t ->
  name:string ->
  (Current_git_test.Commit.t Current.t -> unit Current.t) ->
  (int -> unit) ->
  unit Lwt.t
(** [test ~name pipeline actions] runs [pipeline]. After each interation,
    it calls [actions i] where [i] is the number of the next step ([1] on the
    first call). If [actions i] raises [Exit] then the tests finish. *)

val cancel : string -> unit
(** [cancel msg] cancels the watch named [msg]. *)
