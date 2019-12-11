open Lwt.Infix

type fetch_logs = {
  tail : int option;
}
[@@deriving to_yojson]

type t = {
  pool : Current.Pool.t option;
}

let id = "docker-run"

module Key = struct
  type t = {
    image : Image.t;
    args : string list;
    docker_context : string option;
    fetch_logs : fetch_logs option;
    fetch_files : string list;
  }

  let digest { image; args; docker_context; fetch_logs; fetch_files } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
      "args", [%derive.to_yojson:string list] args;
      "docker_context", [%derive.to_yojson:string option] docker_context;
      "fetch_logs", [%derive.to_yojson:fetch_logs option] fetch_logs;
      "fetch_files", [%derive.to_yojson:string list] fetch_files;
    ]
end

module Value = struct

  type t = {
    logs : string list;
    files : (string * string) list;
  }
  [@@deriving yojson]

  let marshal t =
    Yojson.Safe.to_string (to_yojson t)

  let unmarshal s =
    match of_yojson (Yojson.Safe.from_string s) with
    | Ok t -> t
    | Error e -> failwith e

  let digest = marshal

end

let run_container ~job ~docker_context ~container_name ~image ~args =
  let cmd =
    Cmd.docker ~docker_context @@ ["run"; "--name"; container_name; "-i"; Image.hash image] @ args
  in
  Current.Process.exec ~cancellable:true ~job cmd

let remove_container ~job ~docker_context ~container_name =
  let cmd = Cmd.docker ~docker_context @@ ["rm"; container_name] in
  Current.Process.exec ~cancellable:false ~job cmd

let fetch_logs ~job ~docker_context ~container_name ~tail =
  let cmd =
    let tail = match tail with Some t -> ["--tail"; string_of_int t] | None -> [] in
    Cmd.docker ~docker_context @@ ["logs"] @ tail @ [container_name]
  in
  Current.Process.check_output ~cancellable:true ~job cmd >>= function
  | Ok output -> Lwt.return (Ok (String.split_on_char '\n' output))
  | Error _ as e -> Lwt.return e

let fetch_logs_maybe ~job ~docker_context ~container_name = function
  | Some { tail } -> fetch_logs ~job ~docker_context ~container_name ~tail
  | None -> Lwt.return (Ok [])

let fetch_file ~job ~docker_context ~container_name path =
  (* function read_file taken from plugins/github/api.ml *)
  let read_file path =
    let ch = open_in_bin path in
    Fun.protect
      (fun () ->
         let len = in_channel_length ch in
         really_input_string ch len
      )
      ~finally:(fun () -> close_in ch)
  in
  Current.Process.with_tmpdir (fun dst_path ->
      let dst_path = Fpath.to_string dst_path in
      let cmd =
        let src = container_name ^ ":" ^ path in
        Cmd.docker ~docker_context @@ ["cp"; src; dst_path]
      in
      Current.Process.exec ~cancellable:true ~job cmd >>= function
      | Ok () -> Lwt.return (Ok (read_file dst_path))
      | Error _ as e -> Lwt.return e )

let fetch_files ~job ~docker_context ~container_name =
  let open Lwt_result.Infix in
  let rec loop acc = function
    | [] -> Lwt.return (Ok (List.rev acc))
    | path :: tl ->
      fetch_file ~job ~docker_context ~container_name path >>= fun file ->
      loop ((path, file) :: acc) tl
  in
  loop []

let build { pool } job (Key.{ image; args; docker_context; _ } as key) =
  Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
  let container_name =
    Fmt.str "%a-%s" Current.Job.pp_id (Current.Job.id job) (Image.hash key.image)
  in
  begin
    let open Lwt_result.Infix in
    run_container ~job ~docker_context ~container_name ~image ~args >>= fun () ->
    fetch_logs_maybe ~job ~docker_context ~container_name key.fetch_logs >>= fun logs ->
    fetch_files ~job ~docker_context ~container_name key.fetch_files >>= fun files ->
    Lwt.return (Ok Value.{ logs; files })
  end >>= fun results ->
  remove_container ~job ~docker_context ~container_name >>= fun _ ->
  Lwt.return results

let pp fmt k = Format.fprintf fmt "%s" (Key.digest k)

let auto_cancel = true
