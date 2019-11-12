open Lwt.Infix

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

type t = {
  label : string;
  mutable used : int;
  capacity : int;
  queue : [`Use | `Cancel] Lwt.u Lwt_dllist.t;
}

let create ~label capacity =
  Prometheus.Gauge.set (Metrics.capacity label) (float_of_int capacity);
  { label; used = 0; capacity; queue = Lwt_dllist.create () }

let check t =
  if t.used < t.capacity then (
    match Lwt_dllist.take_opt_l t.queue with
    | None -> ()
    | Some waiter ->
      t.used <- t.used + 1;
      Lwt.wakeup_later waiter `Use
  )

let get ~on_cancel ~switch t =
  let ready, set_ready = Lwt.wait () in
  let node = Lwt_dllist.add_r set_ready t.queue in
  on_cancel (fun _ ->
      Lwt_dllist.remove node;
      if Lwt.is_sleeping ready then Lwt.wakeup_later set_ready `Cancel;
      Lwt.return_unit
    ) >>= fun () ->
  check t;
  let start_wait = Unix.gettimeofday () in
  Prometheus.Gauge.inc_one (Metrics.qlen t.label);
  ready >|= fun ready ->
  Prometheus.Gauge.dec_one (Metrics.qlen t.label);
  match ready with
  | `Cancel -> Fmt.failwith "Cancelled waiting for resource from pool %S" t.label
  | `Use ->
    let stop_wait = Unix.gettimeofday () in
    Prometheus.Summary.observe (Metrics.wait_time t.label) (stop_wait -. start_wait);
    Prometheus.Gauge.inc_one (Metrics.resources_in_use t.label);
    Switch.add_hook_or_fail switch (fun _reason ->
        assert (t.used > 0);
        Prometheus.Gauge.dec_one (Metrics.resources_in_use t.label);
        t.used <- t.used - 1;
        let release_time = Unix.gettimeofday () in
        Prometheus.Summary.observe (Metrics.use_time t.label) (release_time -. stop_wait);
        check t;
        Lwt.return_unit
      )

let pp f t =
  Fmt.string f t.label
