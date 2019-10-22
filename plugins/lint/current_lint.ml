open Current.Syntax
module Docker = Current_docker.Default

let src = Logs.Src.create "current_fmt" ~doc:"Current pipelines for linting"

module Log = (val Logs.src_log src : Logs.LOG)

let option_map f = function Some x -> Some (f x) | None -> None

let version_from_string =
  let re =
    Re.(
      seq
        [
          start;
          rep space;
          str "version";
          rep space;
          char '=';
          rep space;
          group (rep1 @@ diff graph (set "#"));
          rep space;
          eol;
        ]
      |> compile)
  in
  fun path -> Re.exec_opt re path |> option_map (fun g -> Re.Group.get g 1)

let version_from_file path =
  let ( let+ ) = Lwt.Infix.( >>= ) in
  let ( let* ) = Lwt.Infix.( >|= ) in
  if not (Sys.file_exists path) then
    let () = Logs.info (fun m -> m "No .ocamlformat file found") in
    Lwt.return (Ok None)
  else
    let+ channel = Lwt_io.open_file ~mode:Lwt_io.input path in
    let* versions =
      Lwt_io.read_lines channel
      |> Lwt_stream.filter_map version_from_string
      |> Lwt_stream.to_list
    in
    match versions with
    | [ v ] ->
        let () =
          Logs.info (fun m -> m "Found OCamlformat version '%s' in dotfile" v)
        in
        Ok (Some v)
    | _ -> Error (`Msg "Unable to parse .ocamlformat file")

let get_ocamlformat_version ~src =
  Current.component "Infer OCamlformat\nversion"
  |> let> src = src in
     let switch = Current.Switch.create ~label:"blah" () in
     let config = Current.Config.v () in
     let job = Current.Job.create ~switch ~config ~label:"blah" () in
     Current.Input.const
       (Lwt_main.run
          (Current_git.with_checkout ~switch ~job src (fun root ->
               let dotfile = Fpath.(to_string (root / ".ocamlformat")) in
               version_from_file dotfile))
        |> function
        | Ok result -> result
        | Error (`Msg e) -> failwith e)

let format_dockerfile ~base ~ocamlformat_version =
  let open Dockerfile in
  from (Docker.Image.hash base)
  @@ run "git -C /home/opam/opam-repository pull"
  @@ run "opam depext ocamlformat=%s" ocamlformat_version
  @@ run "opam install ocamlformat=%s" ocamlformat_version

let v_from_opam ?ocamlformat_version ~base ~src =
  let ( >|= ) x f = Current.map f x in
  ( match ocamlformat_version with
  | Some v -> Current.return ~label:("version " ^ v) (Some v)
  | None -> get_ocamlformat_version ~src )
  >|= (function Some v -> [ v ] | None -> [])
  |> Current.list_iter ~pp:Fmt.string (fun ocamlformat_version ->
         let dockerfile =
           let+ base = base and+ ocamlformat_version = ocamlformat_version in
           format_dockerfile ~base ~ocamlformat_version
         in
         let img =
           Docker.build ~label:"OCamlformat" ~pull:false ~dockerfile (`Git src)
         in
         Docker.run ~label:"lint" img ~args:[ "dune"; "build"; "@fmt" ])
