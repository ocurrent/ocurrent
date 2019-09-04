(** Like [Lwt_switch], but the cleanup functions are called in sequence, not
    in parallel, and a reason for the shutdown may be given. *)

open Lwt.Infix

type reason = unit Current_term.S.or_error

type callback = reason -> unit Lwt.t

type t = {
  mutable state : [`On of string * callback Stack.t | `Turning_off of reason Lwt.t | `Off of reason];
}

let pp_reason f x = Current_term.Output.pp (Fmt.unit "()") f (x :> unit Current_term.Output.t)

let turn_off t reason =
  match t.state with
  | `Off orig ->
    Log.info (fun f -> f "Switch.turn_off(%a): already off (due to %a)"
                 pp_reason reason
                 pp_reason orig);
    Lwt.return_unit
  | `Turning_off thread ->
    thread >|= fun (_ : reason) ->
    ()
  | `On (_, callbacks) ->
    let th, set_th = Lwt.wait () in
    t.state <- `Turning_off th;
    let rec aux () =
      match Stack.pop callbacks with
      | fn -> fn reason >>= aux
      | exception Stack.Empty ->
        t.state <- `Off reason;
        Lwt.wakeup set_th reason;
        Lwt.return_unit
    in
    aux ()

(* Once the first callback is added, attach a GC finaliser so we can detect if the user
   forgets to turn it off. *)
let gc t =
  match t.state with
  | `Off _ | `Turning_off _ -> ()
  | `On (label, _) ->
    Log.err (fun f -> f "Switch %S GC'd while still on!" label);
    Lwt.async (fun () -> turn_off t @@ Error (`Msg "Switch GC'd while still on!"))

let add_hook_or_fail t fn =
  match t.state with
  | `On (_, callbacks) ->
    if Stack.is_empty callbacks then Gc.finalise gc t;
    Stack.push fn callbacks
  | `Off (Ok ()) -> Fmt.failwith "Switch already off!"
  | `Off (Error (`Msg msg)) -> Fmt.failwith "Switch already off (%s)!" msg
  | `Turning_off _ -> Fmt.failwith "Switch is being turned off!"

let add_hook_or_exec t fn =
  match t.state with
  | `On (_, callbacks) ->
    if Stack.is_empty callbacks then Gc.finalise gc t;
    Stack.push fn callbacks;
    Lwt.return_unit
  | `Off reason ->
    fn reason
  | `Turning_off thread ->
    thread >>= fn

let add_hook_or_exec_opt t fn =
  match t with
  | None -> Lwt.return_unit
  | Some t -> add_hook_or_exec t fn

let create ~label () = {
  state = `On (label, Stack.create ());
}

let create_off reason = {
  state = `Off reason;
}

let is_on t =
  match t.state with
  | `On _ -> true
  | `Off _ | `Turning_off _ -> false

let pp f t =
  match t.state with
  | `On (label, _) -> Fmt.pf f "on(%S)" label
  | `Off r -> Fmt.pf f "off(%a)" pp_reason r
  | `Turning_off _ -> Fmt.string f "turning-off"

let pp_duration f d =
  let d = Duration.to_f d in
  if d > 120.0 then Fmt.pf f "%.1f minutes" (d /. 60.)
  else if d > 2.0 then Fmt.pf f "%.1f seconds" d
  else Fmt.pf f "%f seconds" d

let add_timeout t duration =
  (* We could be smarter about this. e.g. cancel the timeout when the switch is turned off or
     only keep the nearest timeout. *)
  Lwt.async (fun () ->
      Lwt_unix.sleep (Duration.to_f duration) >>= fun () ->
      if is_on t then turn_off t @@ Error (`Msg (Fmt.strf "Timeout (%a)" pp_duration duration))
      else Lwt.return_unit
    )
