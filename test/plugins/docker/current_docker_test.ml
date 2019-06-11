open Current.Syntax

let src = Logs.Src.create "test.docker" ~doc:"OCurrent test docker plugin"
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
  unit Current.Input.t * [`Failed | `Complete] Lwt.u

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
    match Lwt.state ready with
    | Lwt.Sleep ->
      incr ref_count;
      let watch =
        object
          method changed = Lwt.map ignore ready
          method pp f = Key.pp f key
          method release =
            decr ref_count;
            if !ref_count = 0 && Lwt.state ready = Lwt.Sleep then (
              Lwt.wakeup set_ready `Failed
            )
        end
      in
      Error `Pending, [watch]
    | Lwt.Return `Complete -> Ok (), []
    | Lwt.Return `Failed -> Error (`Msg "Failed"), []
    | Lwt.Fail ex -> Error (`Msg (Printexc.to_string ex)), []
  in
  let input = Current.Input.of_fn get in
  let r = (input, set_ready) in
  containers := Containers.add key r !containers;
  input

let run image ~cmd =
  Fmt.strf "docker run @[%a@]" Fmt.(list ~sep:sp string) cmd |>
  let** image = image in
  let key = { Key.image; cmd } in
  Current.track @@
  match Containers.find_opt key !containers with
  | Some (c, _) -> c
  | None -> docker_run key

let complete image ~cmd (r : [ `Complete | `Failed ]) =
  let key = { Key.image; cmd } in
  match Containers.find_opt key !containers with
  | Some (_, s) -> Lwt.wakeup s r
  | _ -> ()

let push image ~tag =
  Fmt.strf "docker push %s" tag |>
  let** _ = image in
  Current.return ()

let reset () =
  containers := Containers.empty

let assert_finished () =
  !containers |> Containers.iter (fun key (_, s) ->
      let ex = Failure (Fmt.strf "Container %a still running!" Key.pp key) in
      try Lwt.wakeup_exn s ex; raise ex
      with Invalid_argument _ -> () (* Already resolved - good! *)
    )
