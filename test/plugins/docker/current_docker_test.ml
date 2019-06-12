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


module Containers = Map.Make(Key)

let containers : (unit, [`Msg of string]) result Lwt.u Containers.t ref = ref Containers.empty

module Run = struct
  type t = No_context
  module Key = Key
  module Value = struct type t = unit end

  let pp = Key.pp

  let build ~switch No_context (key : Key.t) =
    let ready, set_ready = Lwt.wait () in
    containers := Containers.add key set_ready !containers;
    Lwt_switch.add_hook (Some switch) (fun () ->
        if Lwt.state ready = Lwt.Sleep then (
          Lwt.wakeup set_ready @@ Error (`Msg "Cancelled");
        );
        Lwt.return_unit
      );
    ready

  let auto_cancel = true
end

module Run_cache = Current_cache.Make(Run)

let run image ~cmd =
  Fmt.strf "docker run @[%a@]" Fmt.(list ~sep:sp string) cmd |>
  let** image = image in
  let key = { Key.image; cmd } in
  Run_cache.get Run.No_context key

let complete image ~cmd r =
  let key = { Key.image; cmd } in
  match Containers.find_opt key !containers with
  | Some s -> Lwt.wakeup s r
  | None -> Fmt.failwith "Container %a not running!" Key.pp key

let push image ~tag =
  Fmt.strf "docker push %s" tag |>
  let** _ = image in
  Current.return ()

let reset () =
  containers := Containers.empty;
  Run_cache.reset ()

let assert_finished () =
  !containers |> Containers.iter (fun key s ->
      let ex = Failure (Fmt.strf "Container %a still running!" Key.pp key) in
      try Lwt.wakeup_exn s ex; raise ex
      with Invalid_argument _ -> () (* Already resolved - good! *)
    )
