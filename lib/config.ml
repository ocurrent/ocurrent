type t = {
  mutable confirm : Level.t option;
  level_cond : Eio.Condition.t;
}

let set_confirm t level =
  Log.info (fun f -> f "Confirmation threshold is now %a" (Fmt.Dump.option Level.pp) level);
  t.confirm <- level;
  Eio.Condition.broadcast t.level_cond

let get_confirm t = t.confirm

(* If the level isn't changed manually within [duration], remove limiter. *)
let slow_start_thread ~sw t duration =
  Eio.Fiber.fork ~sw
    (fun () ->
       let changed = ref false in
       Eio.Fiber.any [
        (fun () -> Eio_unix.sleep (Duration.to_f duration));
        (fun () -> Eio.Condition.await_no_mutex t.level_cond; changed := true)
       ];
       if !changed = false then (
         Log.info (fun f -> f "Slow start period over; removing limiter");
         set_confirm t None;
       )
    )

let v ?auto_release ?confirm () =
  (* XXX: Hmm, because of the default value and the use of fibers :/ *)
  Eio_main.run @@ fun _ ->
  let level_cond = Eio.Condition.create () in
  let t = { confirm; level_cond } in
  Eio.Switch.run @@ fun sw ->
  Option.iter (slow_start_thread ~sw t) auto_release;
  t

let default = v ()

let active_config : t option Current_incr.var = Current_incr.var None

let now = Current_incr.of_var active_config

let rec confirmed l t =
  match t.confirm with
  | Some threshold when Level.compare l threshold >= 0 ->
    Eio.Condition.await_no_mutex t.level_cond;
    confirmed l t
  | _ -> ()

open Cmdliner

let cmdliner_confirm =
  let levels = List.map (fun l -> Level.to_string l, Some l) Level.values in
  let enum = ("none", None) :: levels in
  let doc =
    Fmt.str
      "Confirm before starting operations at or above this level (%s)."
      (Arg.doc_alts_enum enum)
  in
  Arg.opt (Arg.enum enum) None @@
  Arg.info ~doc ["confirm"]

let auto_release =
  Arg.value @@
  Arg.(opt (some int)) None @@
  Arg.info
    ~doc:"Remove confirm threshold after this many seconds from start-up."
    ~docv:"SEC"
    ["confirm-auto-release"]

let cmdliner =
  let make auto_release confirm =
    let auto_release = Option.map Duration.of_sec auto_release in
    v ?auto_release ?confirm () in
  Term.(const make $ auto_release $ Arg.value cmdliner_confirm)
