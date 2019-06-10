open Current.Syntax

let src = Logs.Src.create "current.test.docker" ~doc:"OCurrent test docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Fpath.t
type image = string

module Key = struct
  type t = {
    image : image;
    cmd : string list;
  }

  let pp f { image; cmd } =
    Fmt.pf f "docker run %s @[%a@]" image Fmt.(list ~sep:sp string) cmd

  let compare = compare
end

module Containers = Map.Make(Key)

type state =
  [ `Running of unit Current.Input.t * unit Lwt.u
  | `Failed
  | `Complete ]

let containers : state Containers.t ref = ref Containers.empty

let build_on platform ~src =
  "build-" ^ platform |>
  let** c = src in
  Current.return @@ Fmt.strf "%s-image-%s" platform (Fpath.to_string c)

let build c =
  "build" |>
  let** c = c in
  Current.return @@ "image-" ^ Fpath.to_string c

let build ?on src =
  match on with
  | None -> build src
  | Some platform -> build_on platform ~src

let docker_run key =
  Log.info (fun f -> f "%a" Key.pp key);
  let ready, set_ready = Lwt.wait () in
  let ref_count = ref 0 in
  let get () =
    incr ref_count;
    let watch =
      object
        method changed = ready
        method pp f = Key.pp f key
        method release =
          decr ref_count;
          if !ref_count = 0 && Lwt.state ready = Lwt.Sleep then (
            Log.info (fun f -> f "Cancelling job %a" Key.pp key);
            containers := Containers.add key `Failed !containers
          )
      end
    in
    Error `Pending, watch
  in
  let input = Current.Input.of_fn get in
  let r = `Running (input, set_ready) in
  containers := Containers.add key r !containers;
  r

let run image ~cmd =
  Fmt.strf "docker run @[%a@]" Fmt.(list ~sep:sp string) cmd |>
  let** image = image in
  let key = { Key.image; cmd } in
  let c =
    match Containers.find_opt key !containers with
    | Some c -> c
    | None -> docker_run key
  in
  match c with
  | `Running (r, _) -> Current.track r
  | `Failed -> Current.fail "Failed"
  | `Complete -> Current.return ()

let complete image ~cmd (r : [ `Complete | `Failed ]) =
  let key = { Key.image; cmd } in
  match Containers.find_opt key !containers with
  | Some `Running (_, s) ->
    let r = (r :> state) in
    containers := Containers.add key r !containers;
    Lwt.wakeup s ()
  | _ -> ()

let push image ~tag =
  Fmt.strf "docker push %s" tag |>
  let** _ = image in
  Current.return ()

let reset () =
  containers := Containers.empty

let assert_finished () =
  !containers |> Containers.iter (fun key state ->
      match state with
      | `Running _ -> Fmt.failwith "Container %a still running!" Key.pp key
      | `Complete | `Failed -> ()
    )
