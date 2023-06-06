val run :
    schedule:Current_cache.Schedule.t ->
    key:string ->
    proc:Eio.Process.mgr ->
    string -> string list Current.t -> unit Current.t
(** [run ~schedule ~key host args] records that [key] is now set to [args], and
    runs ssh host [args] if it has changed.
    e.g. [run ~schedule ~key:"my-ls" "fqdn" ["ls"; "-l"]] *)
