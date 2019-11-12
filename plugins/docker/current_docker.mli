(** Integration with Docker containers. *)

module S = S

module Default : S.DOCKER
(** The default Docker engine (from the [$DOCKER_HOST] environment variable). *)

module Make(Host : S.HOST) : S.DOCKER
(** The docker engine running on [Host]. *)

val push_manifest : ?auth:(string * string) -> tag:string -> S.repo_id Current.t list -> unit Current.t
(** [push_manifest images ~tag] pushes a manifest containing [images] as [tag].
    @param auth If give, do a "docker login" using this username/password pair before pushing. *)
