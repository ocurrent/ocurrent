val check_status :
  ?pp_error_command:(Format.formatter -> unit) ->
  string * string array ->
  Unix.process_status -> unit Current_term.S.or_error

val pp_cmd : Format.formatter -> Lwt_process.command -> unit

val exec :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> Lwt_process.command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> Lwt_process.command ->
  unit Current_term.S.or_error Lwt.t

val check_output :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> Lwt_process.command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> Lwt_process.command ->
  string Current_term.S.or_error Lwt.t

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
