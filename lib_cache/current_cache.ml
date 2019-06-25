open Lwt.Infix
open Current.Syntax

let src = Logs.Src.create "current.cache" ~doc:"OCurrent caching"
module Log = (val Logs.src_log src : Logs.LOG)

let open_temp_file ~dir ~prefix ~suffix =
  let path, ch = Filename.open_temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path, ch

module Job = struct
  type t = Fpath.t * out_channel

  let create ~switch () =
    let jobs_dir = Current.state_dir "job" in
    let time = Unix.gettimeofday () |> Unix.gmtime in
    let date =
      let { Unix.tm_year; tm_mon; tm_mday; _ } = time in
      Fmt.strf "%04d-%02d-%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
    in
    let date_dir = Fpath.(jobs_dir / date) in
    match Bos.OS.Dir.create date_dir with
    | Error (`Msg m) -> failwith m
    | Ok (_ : bool) ->
      let time_prefix =
        let { Unix.tm_hour; tm_min; tm_sec; _ } = time in
        Fmt.strf "%02d%02d%02d_" tm_hour tm_min tm_sec
      in
      let path, ch = open_temp_file ~dir:date_dir ~prefix:time_prefix ~suffix:".log" in
      Log.info (fun f -> f "Created new log file at %a" Fpath.pp path);
      Lwt_switch.add_hook (Some switch) (fun () -> close_out ch; Lwt.return_unit);
      path, ch

  let write (_, ch) msg =
    output_string ch msg;
    flush ch

  let log t fmt =
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
      Unix.gettimeofday () |> Unix.gmtime in
    let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
    Fmt.kstrf (write t) fmt
      (tm_year + 1900) (tm_mon + 1) tm_mday
      tm_hour tm_min tm_sec

  let fd (_, ch) =
    Unix.descr_of_out_channel ch
end

module type BUILDER = sig
  type t

  module Key : sig
    include Set.OrderedType
  end

  module Value : sig
    type t
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

module Make(B : BUILDER) = struct
  module Builds = Map.Make(B.Key)

  let builds : B.Value.t Current.Input.t Builds.t ref = ref Builds.empty

  let invalidate key =
    (* TODO: assert that the build is not currently in progress. *)
    builds := Builds.remove key !builds

  let do_build ~confirmed ctx key =
    let switch = Lwt_switch.create () in
    let ready, set_ready = Lwt.wait () in
    let ref_count = ref 0 in
    let cancel () = Lwt.async (fun () -> Lwt_switch.turn_off switch) in
    let watch =
      object
        method pp f = B.pp f key
        method changed = Lwt.map ignore ready
        method cancel = Some cancel
        method release =
          decr ref_count;
          if !ref_count = 0 && B.auto_cancel && Lwt.state ready = Lwt.Sleep then (
            cancel ();
            invalidate key
          )
      end
    in
    Lwt.async (fun () ->
        let job = Job.create ~switch () in
        Job.log job "Starting build for %a" B.pp key;
        Lwt.finalize
          (fun () ->
             Lwt.try_bind
               (fun () ->
                  confirm confirmed >>= fun () ->
                  B.build ~switch ctx job key
               )
               (fun x ->
                  begin match x with
                    | Ok _ -> Job.log job "Success"
                    | Error (`Msg m) -> Job.log job "Failed: %s" m
                  end;
                  Lwt.wakeup set_ready x;
                  Lwt.return_unit)
               (fun ex ->
                  Job.log job "Error: %a" Fmt.exn ex;
                  Lwt.wakeup_exn set_ready ex;
                  Lwt.return_unit)
          )
          (fun () -> Lwt_switch.turn_off switch)
      );
    Current.Input.of_fn @@ fun () ->
    match Lwt.state ready with
    | Lwt.Sleep -> incr ref_count; Error `Pending, [watch]
    | Lwt.Return x -> (x :> B.Value.t Current_term.Output.t), []
    | Lwt.Fail x -> Error (`Msg (Printexc.to_string x)), []

  let get ctx key =
    let level = B.level ctx key in
    let* confirmed = Current.confirmed level in
    Current.track @@
    match Builds.find_opt key !builds with
    | Some b -> b
    | None ->
      let b = do_build ~confirmed:(confirmed, level) ctx key in
      builds := Builds.add key b !builds;
      b

  let reset () =
    builds := Builds.empty
end
