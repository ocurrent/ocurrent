val test_commit : Current_git_test.Commit.t

val test :
  name:string ->
  (Current_git_test.Commit.t Current.t -> unit Current.t) ->
  (int -> unit) ->
  unit
(** [test ~name pipeline actions] runs [pipeline]. After each interation,
    it calls [actions i] where [i] is the number of the next step ([1] on the
    first call). If [actions i] raises [Exit] then the tests finish. *)
