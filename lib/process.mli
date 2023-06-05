type command = string * string list

val pp_cmd : Format.formatter -> command -> unit

val exec :
  ?cwd:Eio.Fs.dir Eio.Path.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> #Eio.Process.mgr -> command ->
  unit Current_term.S.or_error

val check_output :
  ?cwd:Eio.Fs.dir Eio.Path.t -> ?stdin:string ->
  ?pp_cmd:(Format.formatter -> command -> unit) ->
  ?pp_error_command:(Format.formatter -> unit) ->
  cancellable:bool -> job:Job.t -> #Eio.Process.mgr -> command ->
  string Current_term.S.or_error

val with_tmpdir : ?prefix:string -> Eio.Fs.dir Eio.Path.t -> (Eio.Fs.dir Eio.Path.t -> 'a) -> 'a
