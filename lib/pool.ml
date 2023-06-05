module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "pool"

  let qlen =
    let help = "Number of users waiting for a resource" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "qlen"

  let wait_time =
    let help = "Time spent waiting for a resource" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "wait_time_seconds"

  let use_time =
    let help = "Time spent using a resource" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "use_time_seconds"

  let resources_in_use =
    let help = "Number of resources currently being used" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "resources_in_use"

  let capacity =
    let help = "Total pool capacity" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "capacity"
end

type priority = [ `High | `Low ]

type 'a t = {
  name : string;
  get : sw:Eio.Switch.t -> priority:priority -> 'a Eio.Promise.or_exn * (unit -> unit);
}

module Local = struct
  type t = {
    label : string;
    mutable used : int;
    capacity : int;
    queue_low : [`Use | `Cancel] Eio.Promise.u Lwt_dllist.t;
    queue_high : [`Use | `Cancel] Eio.Promise.u Lwt_dllist.t;
  }

  let check t =
    if t.used < t.capacity then (
      let next =
        match Lwt_dllist.take_opt_l t.queue_high with
        | None -> Lwt_dllist.take_opt_l t.queue_low
        | Some _ as x -> x
      in
      next |> Option.iter @@ fun waiter ->
      t.used <- t.used + 1;
      Eio.Promise.resolve waiter `Use
    )

  let get t ~sw ~priority =
    let ready, set_ready = Eio.Promise.create () in
    let queue =
      match priority with
      | `High -> t.queue_high
      | `Low -> t.queue_low
    in
    let node = Lwt_dllist.add_r set_ready queue in
    let cancel () =
        Lwt_dllist.remove node;
        if not (Eio.Promise.is_resolved ready) then Eio.Promise.resolve set_ready `Cancel
    in
    check t;
    let start_wait = Unix.gettimeofday () in
    Prometheus.Gauge.inc_one (Metrics.qlen t.label);
    let th =
      (* XXX: Could this just be a thunk? *)
      Eio.Fiber.fork_promise ~sw (fun () ->
        (* Switch.add_hook_or_exec switch cancel; *)
        let ready = Eio.Promise.await ready in
        Prometheus.Gauge.dec_one (Metrics.qlen t.label);
        match ready with
        | `Cancel -> Fmt.failwith "Cancelled waiting for resource from pool %S" t.label
        | `Use ->
          let stop_wait = Unix.gettimeofday () in
          Prometheus.Summary.observe (Metrics.wait_time t.label) (stop_wait -. start_wait);
          Prometheus.Gauge.inc_one (Metrics.resources_in_use t.label);
          Eio.Switch.on_release sw (fun _reason ->
            assert (t.used > 0);
            Prometheus.Gauge.dec_one (Metrics.resources_in_use t.label);
            t.used <- t.used - 1;
            let release_time = Unix.gettimeofday () in
            Prometheus.Summary.observe (Metrics.use_time t.label) (release_time -. stop_wait);
            check t
          )
      )
    in
    th, cancel

  let create ~label capacity =
    Prometheus.Gauge.set (Metrics.capacity label) (float_of_int capacity);
    let t = { label; used = 0; capacity;
      queue_low = Lwt_dllist.create ();
      queue_high = Lwt_dllist.create ()
    } in
    { name = label; get = get t }
end

let create = Local.create

let of_fn ~label get = { name = label; get }

let get t = t.get

let pp f t =
  Fmt.string f t.name
