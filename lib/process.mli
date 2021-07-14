val exec :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> Lwt_process.command ->
  unit Current_term.S.or_error Lwt.t

val check_output :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> Lwt_process.command ->
  string Current_term.S.or_error Lwt.t

val exec_with :
  ?cwd:Fpath.t ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> Lwt_process.command ->
  (Lwt_process.process_full -> 'a Current_term.S.or_error Lwt.t) ->
  'a Current_term.S.or_error Lwt.t

val with_tmpdir : ?prefix:string -> (Fpath.t -> 'a Lwt.t) -> 'a Lwt.t
