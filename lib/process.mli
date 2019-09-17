val exec :
  ?switch:Switch.t -> ?stdin:string ->
  ?pp_error_command:(Format.formatter -> unit) ->
  job:Job.t -> Lwt_process.command ->
  unit Current_term.S.or_error Lwt.t

val check_output :
  ?switch:Switch.t -> ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_error_command:(Format.formatter -> unit) ->
  job:Job.t -> Lwt_process.command ->
  string Current_term.S.or_error Lwt.t

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
