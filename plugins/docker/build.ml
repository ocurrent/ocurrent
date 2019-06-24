open Lwt.Infix

type t = No_context

module Key = struct
  type t = {
    commit : Current_git.Commit.t;
    dockerfile : string option;
  } [@@deriving ord]

  let id { commit; dockerfile } =
    let dockerfile =
      match dockerfile with
      | None -> ""
      | Some contents -> "-" ^ (Digest.string contents |> Digest.to_hex)
    in
    Current_git.Commit.id commit ^ dockerfile

  let pp f t = Fmt.string f (id t)
end

module Value = Image

let docker_tag t = Fmt.strf "build-of-%s" (Key.id t)

let send_dockerfile ~dockerfile ch =
  Lwt.try_bind
    (fun () ->
       match dockerfile with
       | None -> Lwt_io.close ch
       | Some contents ->
         Lwt_io.write ch contents >>= fun () ->
         Lwt_io.close ch
    )
    (fun () -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Error ex))

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let build ~switch No_context key =
  let { Key.commit; dockerfile } = key in
  let tag = docker_tag key in
  Current_git.with_checkout commit @@ fun dir ->
  let f =
    match dockerfile with
    | None -> "Dockerfile"
    | Some _ -> "-"
  in
  let cmd = [| "docker"; "build"; "-f"; f; "-t"; tag; "--"; Fpath.to_string dir |] in
  let proc = Lwt_process.open_process_out ("", cmd) in
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      if proc#state = Lwt_process.Running then (
        Log.info (fun f -> f "Cancelling build of %a" Current_git.Commit.pp commit);
        proc#terminate;
      );
      Lwt.return_unit
    )
  >>= fun () ->
  send_dockerfile ~dockerfile proc#stdin >>= fun sent_df ->
  proc#status >|= function
  | Unix.WEXITED 0 ->
    begin match sent_df with
      | Error ex -> errorf "Build succeeded, but sending Dockerfile failed! %a" Fmt.exn ex
      | Ok () ->
        Log.info (fun f -> f "Build of docker image %S succeeded" tag);
        Ok tag
    end
  | Unix.WEXITED n ->
    errorf "Build failed with exit status %d" n
  | Unix.WSIGNALED s ->
    errorf "Build failed with signal %d" s
  | Unix.WSTOPPED x ->
    errorf "Expected exit status: stopped with %d" x

let pp f key = Fmt.pf f "docker build %a" Key.pp key

let auto_cancel = true

let level _ _ = Current.Level.Average
