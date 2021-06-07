let serve_file ~docroot uri =
  let fname = Utils.Server.resolve_local_file ~docroot ~uri in
  Utils.Server.respond_file ~fname ()

(*
  Serve files directly out of the given docroot.

  Note that we ignore the given path, and we use the Context.uri instead.
  The routes package has added a wildcard option
  (https://github.com/anuragsoni/routes/pull/118) but this has not been
  released yet.  We're just using a string for matching for now, which
  we discard.
 *)
let r ~docroot _path =
  object
    inherit Resource.t

    method! private get ctx =
      let uri = Context.uri ctx in
      serve_file ~docroot uri
  end
