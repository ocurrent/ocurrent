open Prometheus

let namespace = "ocurrent"
let subsystem = "docker"

let docker_pull_events =
  let help = "Incoming docker pull events" in
  Gauge.v ~help ~namespace ~subsystem "pull_events"

let docker_push_events =
  let help = "Outgoing docker push events" in
  Gauge.v ~help ~namespace ~subsystem "push_events"

let docker_push_manifest_events =
  let help = "Outgoing docker push manifest events" in
  Gauge.v ~help ~namespace ~subsystem "push_manifest_events"

let docker_peek_events =
  let help = "Incoming docker peek events" in
  Gauge.v ~help ~namespace ~subsystem "peek_events"

let docker_build_events =
  let help = "Docker build events" in
  Gauge.v ~help ~namespace ~subsystem "build_events"
