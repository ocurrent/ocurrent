module type BUILDER = sig
  type t

  module Key : sig
    include Set.OrderedType
  end

  module Value : sig
    type t
  end

  val pp : Key.t Fmt.t

  val build : switch:Lwt_switch.t -> t -> Key.t -> (Value.t, [`Msg of string]) result Lwt.t

  val auto_cancel : bool
end

module Make(B : BUILDER) = struct
  module Builds = Map.Make(B.Key)

  let builds : B.Value.t Current.Input.t Builds.t ref = ref Builds.empty

  let do_build ctx key =
    let switch = Lwt_switch.create () in
    let ready, set_ready = Lwt.wait () in
    let ref_count = ref 0 in
    let watch =
      object
        method pp f = B.pp f key
        method changed = Lwt.map ignore ready
        method release =
          decr ref_count;
          if !ref_count = 0 && B.auto_cancel then
            Lwt.async (fun () -> Lwt_switch.turn_off switch)
      end
    in
    Lwt.async (fun () ->
        Lwt.try_bind
          (fun () -> B.build ~switch ctx key)
          (fun x -> Lwt.wakeup set_ready x; Lwt.return_unit)
          (fun ex -> Lwt.wakeup_exn set_ready ex; Lwt.return_unit)
      );
    Current.Input.of_fn @@ fun () ->
    match Lwt.state ready with
    | Lwt.Sleep -> incr ref_count; Error `Pending, [watch]
    | Lwt.Return x -> (x :> B.Value.t Current_term.Output.t), []
    | Lwt.Fail x -> Error (`Msg (Printexc.to_string x)), []

  let get ctx key =
    Current.track @@
    match Builds.find_opt key !builds with
    | Some b -> b
    | None ->
      let b = do_build ctx key in
      builds := Builds.add key b !builds;
      b

  let reset () =
    builds := Builds.empty
end
