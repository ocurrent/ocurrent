open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "pool"

  let qlen =
    let help = "Number of users waiting for a resource" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "qlen"

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
  let rec aux () =
    if t.used < t.capacity then (
      if Switch.is_on switch then (
        Prometheus.Gauge.inc_one (Metrics.resources_in_use t.label);
        t.used <- t.used + 1;
        Switch.add_hook_or_exec switch (fun _reason ->
            assert (t.used > 0);
            Prometheus.Gauge.dec_one (Metrics.resources_in_use t.label);
            t.used <- t.used - 1;
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
