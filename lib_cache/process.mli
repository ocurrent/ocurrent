val exec : job:Job.t -> Lwt_process.command -> unit Current.or_error Lwt.t

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
