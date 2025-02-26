type auth = {
  user : string;
  password : string;
  server : string option;
}

let v ~auth ~server =
  Option.map (fun (user, password) -> { user; password; server }) auth

let login ?config ~docker_context ~job = function
  | None -> Lwt.return (Ok ())
  | Some { user; password; server } ->
      let cmd = Cmd.login ?config ~docker_context ?server user in
      Current.Process.exec ~cancellable:true ~job ~stdin:password cmd
