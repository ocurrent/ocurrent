module Config = Sandmark.Config

val sandmark : pool:Current.Pool.t -> config:Config.t -> Current_git.Commit.t Current.t -> unit Current.t
(** [sandmark ~pool ~config commit] evaluates to the result of benchmarking [commit]. *)

val recent_commits : n:int -> Current_git.Commit.t Current.t -> Current_git.Commit.t list Current.t
(** [recent_commits ~n commit] is up to [n] commits leading to (and including) [commit]. *)
