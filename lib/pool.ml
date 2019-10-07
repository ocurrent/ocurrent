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
  cond : unit Lwt_condition.t;
}

let create ~label capacity =
  Prometheus.Gauge.set (Metrics.capacity label) (float_of_int capacity);
  { label; used = 0; capacity; cond = Lwt_condition.create () }

let get ~switch t =
  let start_wait = Unix.gettimeofday () in
  let rec aux () =
    if t.used < t.capacity then (
      if Switch.is_on switch then (
        let stop_wait = Unix.gettimeofday () in
        Prometheus.Summary.observe (Metrics.wait_time t.label) (stop_wait -. start_wait);
        Prometheus.Gauge.inc_one (Metrics.resources_in_use t.label);
        t.used <- t.used + 1;
        Switch.add_hook_or_exec switch (fun _reason ->
            assert (t.used > 0);
            Prometheus.Gauge.dec_one (Metrics.resources_in_use t.label);
            t.used <- t.used - 1;
            let release_time = Unix.gettimeofday () in
            Prometheus.Summary.observe (Metrics.use_time t.label) (release_time -. stop_wait);
            Lwt_condition.broadcast t.cond ();
            Lwt.return_unit
          )
      ) else Fmt.failwith "Cancelled waiting for resource from pool %S" t.label
    ) else (
      Prometheus.Gauge.inc_one (Metrics.qlen t.label);
      Lwt_condition.wait t.cond >>= fun () ->
      Prometheus.Gauge.dec_one (Metrics.qlen t.label);
      aux ()
    )
  in
  let res = aux () in
  Switch.add_hook_or_exec switch (fun _ex -> Lwt.cancel res; Lwt.return_unit) >>= fun () ->
  res

let pp f t =
  Fmt.string f t.label
