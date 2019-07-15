module Config = Sandmark.Config

type pool

val create_pool : string list -> pool
(** [create_pool workers] is a pool containing the benchmark machines [workers]. *)

val sandmark : workers:pool -> config:Config.t -> Current_git.Commit.t Current.t -> unit Current.t
(** [sandmark ~workers ~config commit] evaluates to the result of benchmarking [commit] using a
    worker from [workers]. *)

val recent_commits : n:int -> Current_git.Commit.t Current.t -> Current_git.Commit.t list Current.t
(** [recent_commits ~n commit] is up to [n] commits leading to (and including) [commit]. *)
