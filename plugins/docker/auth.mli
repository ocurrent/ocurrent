type auth

val v : auth:(string * string) option -> server:string option -> auth option

val login : ?config:Fpath.t -> docker_context:string option -> job:Current.Job.t -> auth option -> unit Current.or_error Lwt.t
