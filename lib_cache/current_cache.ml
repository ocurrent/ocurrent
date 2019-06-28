open Lwt.Infix
open Current.Syntax

let timestamp = ref Unix.gettimeofday
let sleep = ref Lwt_unix.sleep

module type BUILDER = sig
  type t

  val id : string

  module Key : sig
    type t
    val digest : t -> string
  end

  module Value : sig
    type t
    val marshal : t -> string
    val unmarshal : string -> t
  end

  val pp : Key.t Fmt.t

  val build : switch:Lwt_switch.t -> t -> Job.t -> Key.t -> (Value.t, [`Msg of string]) result Lwt.t

  val auto_cancel : bool

  val level : t -> Key.t -> Current.Level.t
end

let confirm (confirmed, level) =
  match Lwt.state confirmed with
  | Lwt.Return () -> Lwt.return_unit
  | _ ->
    Log.info (fun f -> f "Waiting for confirm-level >= %a" Current.Level.pp level);
    confirmed >|= fun () ->
    Log.info (fun f -> f "Confirm-level now >= %a" Current.Level.pp level)

module Schedule = struct
  type t = {
    valid_for : Duration.t option;
  }

  let v ?valid_for () = { valid_for }

  let default = v ()
end

let durations = Duration.[
  to_year, "years";
  to_day, "days";
  to_hour, "hours";
  to_min, "minutes";
  to_sec, "seconds"
]

let pp_duration_rough f d =
  let rec aux = function
    | [] -> Fmt.pf f "%.3f s" (Duration.to_f d)
    | (to_int, units) :: xs ->
      let i = to_int d in
      if i < 5 then aux xs
      else Fmt.pf f "%d %s" i units
  in
  aux durations

module Make(B : BUILDER) = struct
  module Builds = Map.Make(String)

  type build_result = B.Value.t Current.or_error * float  (* The final result and end time *)

  type build = {
    key : B.Key.t;
    switch : Lwt_switch.t;          (* Turning this off aborts the build. *)
    finished : build_result Lwt.t;  (* Unresolved if the build is still in progress. *)
    mutable auto_cancelled : bool;  (* Don't record the result (because the build was auto-cancelled). *)
    mutable ref_count : int;        (* The number of watchers waiting for the result (for auto-cancel). *)
  }

  let builds : build Builds.t ref = ref Builds.empty

  let is_running build = Lwt.state build.finished = Lwt.Sleep

  let force_invalidate key =
    builds := Builds.remove key !builds;
    Db.invalidate ~builder:B.id key

  (* Mark this build as invalid. The caller is responsible for triggering a re-evaluation. *)
  let invalidate key =
    let key_digest = B.Key.digest key in
    match Builds.find_opt key_digest !builds with
    | Some build when is_running build -> Fmt.failwith "invalidate(%a): build is still running!" B.pp key
    | _ -> force_invalidate key_digest

  (* This thread runs in the background to perform the build.
     When done, it records the result in the database (unless it was auto-cancelled). *)
  let do_build ~confirmed ctx build =
    let job = Job.create ~switch:build.switch ~id:B.id () in
    let log = Job.log_id job in
    let ready = !timestamp () |> Unix.gmtime in
    let running = ref None in
    Job.log job "Starting build for %a" B.pp build.key;
    Lwt_switch.add_hook (Some build.switch) (fun () ->
        if build.auto_cancelled then (
          Job.log job "Auto-cancelling job because it is no longer needed";
        ) else if is_running build then (
          Job.log job "Cancelling job"
        );
        Lwt.return_unit
      );
    confirm confirmed >>= fun () ->
    running := Some (!timestamp () |> Unix.gmtime);
    Lwt.catch
      (fun () -> B.build ~switch:build.switch ctx job build.key)
      (fun ex -> Lwt.return @@ Error (`Msg (Printexc.to_string ex)))
    >|= fun x ->
    let finished = !timestamp () in
    if build.auto_cancelled then (
      force_invalidate (B.Key.digest build.key);
      (* If anyone started wanting this build again after it got cancelled,
         then they will now recalculate, discover the build doesn't exist, and
         trigger a new one. *)
      Error (`Msg "Auto-cancelled"), finished
    ) else (
      let record result =
        Db.record
          ~builder:B.id
          ~key:(B.Key.digest build.key)
          ~log
          ~ready ~running:!running ~finished:(Unix.gmtime finished)
          result;
        x, finished
      in
      match x with
      | Ok v ->
        Job.log job "Success";
        record @@ Ok (B.Value.marshal v)
      | Error (`Msg m) as e ->
        Job.log job "Failed: %s" m;
        record e
    )

  (* Create a new in-memory build object. Try to initialise it with the value from the disk-cache.
     If there isn't one, start a new build in a background thread. *)
  let get_build ~confirmed ctx key =
    let key_digest = B.Key.digest key in
    let switch = Lwt_switch.create () in
    let finished, set_finished = Lwt.wait () in
    let build = { switch; key; finished; ref_count = 0; auto_cancelled = false } in
    match Db.lookup ~builder:B.id key_digest with
    | Some entry ->
      Log.info (fun f -> f "Loaded cached result for %a" B.pp key);
      let v =
        match entry.Db.value with
        | Ok v -> Ok (B.Value.unmarshal v)
        | Error _ as e -> e
      in
      Lwt.wakeup set_finished (v, entry.Db.finished);
      build
    | None ->
      Lwt.async (fun () ->
          Lwt.try_bind
            (fun () ->
               Lwt.finalize
                 (fun () -> do_build ~confirmed ctx build)
                 (fun () -> Lwt_switch.turn_off build.switch)
            )
            (fun result -> Lwt.wakeup set_finished result; Lwt.return_unit)
            (fun ex -> Lwt.wakeup_exn set_finished ex; Lwt.return_unit)
        );
      build

  (* The build must be currently running when calling this. *)
  let input_running build =
    build.ref_count <- build.ref_count + 1;
    let cancel () = Lwt.async (fun () -> Lwt_switch.turn_off build.switch) in
    let watch =
      object
        method pp f = B.pp f build.key
        method changed = Lwt.map ignore build.finished
        method cancel = Some cancel
        method release =
          build.ref_count <- build.ref_count - 1;
          if build.ref_count = 0 && B.auto_cancel && is_running build then (
            build.auto_cancelled <- true;
            cancel ()
          )
      end
    in
    Error `Pending, [watch]

  (* The build must be finished when calling this. *)
  let input_finished ~schedule build (value, finished) =
    match schedule.Schedule.valid_for with
    | None -> (value :> B.Value.t Current_term.Output.t), []
    | Some duration ->
      let remaining_time = Duration.to_f duration +. finished -. !timestamp () in
      let changed =
        if remaining_time <= 0.0 then (
          Log.info (fun f -> f "Build result for %a has expired" B.pp build.key);
          invalidate build.key;
          Lwt.return_unit       (* Trigger an immediate recalculation *)
        ) else (
          !sleep remaining_time
        )
      in
      let watch =
        object
          method pp f =
            if remaining_time <= 0.0 then
              Fmt.pf f "%a (expired)" B.pp build.key
            else
              Fmt.pf f "%a will be invalid after %a"
                B.pp build.key
                pp_duration_rough (Duration.of_f remaining_time)

          method changed = changed
          method cancel = None
          method release = ()
        end
      in
      (value :> B.Value.t Current_term.Output.t), [watch]

  let input build ~schedule () =
    match Lwt.state build.finished with
    | Lwt.Sleep -> input_running build
    | Lwt.Return v -> input_finished build ~schedule v
    | Lwt.Fail ex -> raise ex

  let get ?(schedule=Schedule.default) ctx key =
    let level = B.level ctx key in
    let* confirmed = Current.confirmed level in
    Current.track @@
    let key_digest = B.Key.digest key in
    let b =
      match Builds.find_opt key_digest !builds with
      | Some b -> b
      | None ->
        let b = get_build ~confirmed:(confirmed, level) ctx key in
        builds := Builds.add key_digest b !builds;
        b
    in
    Current.Input.of_fn (input b ~schedule)

  let reset () =
    builds := Builds.empty;
    Db.drop_all B.id
end

module Job = Job
module Process = Process
