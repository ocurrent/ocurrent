open Current.Syntax

let src = Logs.Src.create "test.docker" ~doc:"OCurrent test docker plugin"
module Log = (val Logs.src_log src : Logs.LOG)

type source = Fpath.t

module Image = struct
  type t = string
  let pp = Fmt.string
  let digest t = t
end

module Key = struct
  type t = {
    image : Image.t;
    cmd : string list;
  }

  let pp f { image; cmd } =
    Fmt.pf f "docker run %S @[%a@]" image Fmt.(list ~sep:sp (quote string)) cmd

  let compare = compare

  let digest = Fmt.to_to_string pp
end

let build_on platform ~src =
  Current.component "build-%s" platform |>
  let> c = src in
  Current.Primitive.const @@ Fmt.str "%s-image-%s" platform (Fpath.to_string c)

let build c =
  Current.component "build" |>
  let> c = c in
  Current.Primitive.const @@ "image-" ^ Fpath.to_string c

let build ?on src =
  match on with
  | None -> build src
  | Some platform -> build_on platform ~src


module Containers = Map.Make(Key)

let containers : (((unit, [`Msg of string]) result), exn) result Eio.Promise.u Containers.t ref = ref Containers.empty

module Run = struct
  type t = No_context
  module Key = Key
  module Value = Current.Unit

  let id = "docker-run"

  let pp = Key.pp

  let build No_context job (key : Key.t) =
    Current.Job.start job ~level:Current.Level.Average;
    let ready, set_ready = Eio.Promise.create () in
    containers := Containers.add key set_ready !containers;
    Current.Job.on_cancel job (fun m ->
        if not (Eio.Promise.is_resolved ready) then Eio.Promise.resolve set_ready @@ Ok (Error (`Msg m));
    );
    Eio.Promise.await_exn ready

  let auto_cancel = true
end

module Run_cache = Current_cache.Make(Run)

let run ~sw image ~cmd =
  Current.component "docker run @[%a@]" Fmt.(list ~sep:sp string) cmd |>
  let> image = image in
  let key = { Key.image; cmd } in
  Run_cache.get ~sw No_context key

let complete image ~cmd r =
  let key = { Key.image; cmd } in
  match Containers.find_opt key !containers with
  | Some s -> Eio.Promise.resolve_ok s r
  | None -> Fmt.failwith "Container %a not running!" Key.pp key

module Push = struct
  type t = No_context
  module Key = Image
  module Value = Current.Unit

  let id = "docker-push"

  let pp f k = Fmt.pf f "docker push %a" Key.pp k

  let build No_context job _key =
    Current.Job.start job ~level:Current.Level.Dangerous;
    Ok ()

  let auto_cancel = false
end

module Push_cache = Current_cache.Make(Push)

let push ~sw image ~tag =
  Current.component "docker push %s" tag |>
  let> image = image in
  Push_cache.get ~sw No_context image

let image_pulls : (string, 'a Current.or_error Eio.Promise.t * 'a Current.or_error Eio.Promise.u) Hashtbl.t = Hashtbl.create 5
let image_monitors = Hashtbl.create 5
let pulls_cond = Eio.Condition.create ()

let get_pull tag =
  match Hashtbl.find_opt image_pulls tag with
  | Some x -> x
  | None ->
    let x = Eio.Promise.create () in
    Hashtbl.add image_pulls tag x;
    x

let image_monitor ~sw tag =
  match Hashtbl.find_opt image_monitors tag with
  | Some x -> x
  | None ->
    let read () = fst @@ get_pull tag in
    let watch refresh =
      let rec aux () =
        Eio.Condition.await_no_mutex pulls_cond;
        refresh ();
        aux ()
      in
      let cancel_cond = Eio.Condition.create () in
      let cancel () =
        Eio.Condition.await_no_mutex cancel_cond;
        raise (Eio.Cancel.Cancelled (Failure "Stopping monitor"))
      in
      Eio.Fiber.fork ~sw (fun () -> try Eio.Fiber.both aux cancel with Eio.Cancel.Cancelled _ -> ());
      (fun () -> Eio.Condition.broadcast cancel_cond)
    in
    let pp f = Fmt.string f "docker pull" in
    let x = Current.Monitor.create ~sw ~read ~watch ~pp in
    Hashtbl.add image_monitors tag x;
    x

let pull ~sw tag =
  Current.component "docker pull %s" tag |>
  let> () = Current.return () in
  Current.Monitor.get (image_monitor ~sw tag)

let complete_pull tag (image : Image.t Current.or_error) =
  match Hashtbl.find_opt image_pulls tag with
  | None -> Fmt.failwith "Image %S isn't being pulled!" tag
  | Some (_, set_image) -> Eio.Promise.resolve set_image image

let update_pull tag =
  Hashtbl.remove image_pulls tag;
  ignore @@ get_pull tag;
  Eio.Condition.broadcast pulls_cond

let reset () =
  containers := Containers.empty;
  Hashtbl.clear image_pulls;
  Hashtbl.clear image_monitors;
  Run_cache.reset ~db:true;
  Push_cache.reset ~db:true

let assert_finished () =
  !containers |> Containers.iter (fun key s ->
      let ex = Failure (Fmt.str "Container %a still running!" Key.pp key) in
      try Eio.Promise.resolve_error s ex; raise ex
      with Invalid_argument _ -> () (* Already resolved - good! *)
    )
