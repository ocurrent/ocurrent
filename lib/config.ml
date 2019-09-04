open Lwt.Infix

type t = {
  mutable confirm : Level.t option;
  level_cond : unit Lwt_condition.t;
}

let v ?confirm () =
  let level_cond = Lwt_condition.create () in
  { confirm; level_cond }

let default = v ()

let set_confirm t level =
  t.confirm <- level;
  Lwt_condition.broadcast t.level_cond ()

let get_confirm t = t.confirm

let rec confirmed l t =
  match t.confirm with
  | Some threshold when Level.compare l threshold >= 0 ->
    Lwt_condition.wait t.level_cond >>= fun () ->
    confirmed l t
  | _ ->
    Lwt.return_unit

open Cmdliner

let cmdliner_confirm =
  let levels = List.map (fun l -> Level.to_string l, Some l) Level.values in
  let conv = Arg.enum @@ ("none", None) :: levels in
  Arg.opt conv None @@
  Arg.info ~doc:"Confirm before starting operations at or above this level."
    ["confirm"]

let cmdliner =
  let make confirm = v ?confirm () in
  Term.(const make $ Arg.value cmdliner_confirm)
