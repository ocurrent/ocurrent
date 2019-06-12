val test :
  name:string ->
  (Current_git_test.Commit.t Current.t -> unit Current.t) ->
  unit
