open Lwt.Infix
open Current.Syntax

let src = Logs.Src.create "current.docker" ~doc:"OCurrent docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Fpath.t
type image = string

module Key : sig
  include Set.OrderedType
  val of_path : Fpath.t -> t
  val dir : t -> Fpath.t
  val docker_tag : t -> string
  val pp : t Fmt.t
end = struct
  type t = Fpath.t

  let of_path t =
    if Fpath.is_abs t then t
    else Fpath.normalize @@ Fpath.append (Fpath.v @@ Sys.getcwd ()) t

  let compare = Fpath.compare

  let pp = Fpath.pp

  let dir t = t

  let docker_tag t =
    let hash = Fpath.to_string t |> Digest.string |> Digest.to_hex in
    Fmt.strf "build-of-%s-%s" (Fpath.basename t) hash
end

module Builds = Map.Make(Key)

type build = image Current.Input.t

let builds : build Builds.t ref = ref Builds.empty

let do_build (key : Key.t) =
  let tag = Key.docker_tag key in
  let cmd = [| "docker"; "build"; "-t"; tag; "--"; Fpath.to_string (Key.dir key) |] in
  let proc = Lwt_process.open_process_none ("", cmd) in
  let ready, set_ready = Lwt.wait () in
  Lwt.async (fun () ->
      proc#status >|= function
      | Unix.WEXITED 0 ->
        Log.info (fun f -> f "Build of docker image %S succeeded" tag);
        Lwt.wakeup set_ready ()
      | Unix.WEXITED n ->
        let msg = Fmt.strf "Build failed with exit status %d" n in
        Lwt.wakeup_exn set_ready (Failure msg)
      | Unix.WSIGNALED s ->
        let msg = Fmt.strf "Build failed with signal %d" s in
        Lwt.wakeup_exn set_ready (Failure msg)
      | Unix.WSTOPPED x -> Fmt.failwith "Expected exit status: stopped with %d" x
    );
  Current.Input.of_fn @@ fun () ->
  match Lwt.state ready with
  | Lwt.Sleep ->
    let watch =
      object
        method pp f = Fmt.pf f "docker build %a" Key.pp key
        method changed = ready
        method release = ()
      end
    in
    Error `Pending, [watch]
  | Lwt.Return () -> Ok tag, []
  | Lwt.Fail (Failure msg) -> Error (`Msg msg), []
  | Lwt.Fail x -> Error (`Msg (Printexc.to_string x)), []

let build c =
  "build" |>
  let** c = c in
  let key = Key.of_path c in
  let b =
    match Builds.find_opt key !builds with
    | Some b -> b
    | None ->
      let b = do_build key in
      builds := Builds.add key b !builds;
      b
  in
  Current.track b
