val exec :
  ?switch:Lwt_switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
  unit Current.or_error Lwt.t

val check_output :
  ?switch:Lwt_switch.t -> ?stdin:string -> job:Job.t -> Lwt_process.command ->
  string Current.or_error Lwt.t

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
