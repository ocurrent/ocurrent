val pp_cmd : Format.formatter -> Lwt_process.command -> unit

val exec :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> Lwt_process.command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> string * string array ->
  unit Current_term.S.or_error

val check_output :
  ?cwd:Fpath.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> Lwt_process.command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> string * string array ->
  string Current_term.S.or_error

val with_tmpdir : ?prefix:string -> (Fpath.t -> unit) -> unit
