(** Integration with Docker containers. *)

type source = Current_git.Commit.t

module S = S

module Default : S.DOCKER
(** The default Docker engine (from the [$DOCKER_HOST] environment variable). *)

module Make(Host : S.HOST) : S.DOCKER
(** The docker engine running on [Host]. *)
