(* RPC client for OCurrent pipelines.

   This client can connect to any OCurrent-based application that exposes
   an RPC endpoint via --capnp-address.

   Examples:

   # Show pipeline overview
   $ rpc_client overview --connect=./engine.cap

   # List active jobs
   $ rpc_client jobs -c ./engine.cap

   # Get job status
   $ rpc_client status -c ./engine.cap 2024-01-15/123456-docker-build-abc123

   # Stream job log
   $ rpc_client log -c ./engine.cap 2024-01-15/123456-docker-build-abc123

   # Cancel a job
   $ rpc_client cancel -c ./engine.cap 2024-01-15/123456-docker-build-abc123

   # Rebuild a job
   $ rpc_client rebuild -c ./engine.cap 2024-01-15/123456-docker-build-abc123

   # Query job history
   $ rpc_client query -c ./engine.cap --ok=false --prefix=2024-01-15

   # Get pipeline DOT graph
   $ rpc_client dot -c ./engine.cap | dot -Tsvg > pipeline.svg

   # Get/set confirmation level
   $ rpc_client confirm -c ./engine.cap
   $ rpc_client confirm -c ./engine.cap --set=dangerous

   # Rebuild multiple jobs
   $ rpc_client rebuild-all -c ./engine.cap job1 job2 job3
*)

let () = Prometheus_unix.Logging.init ~default_level:Logs.Warning ()

let () =
  let cmd = Current_rpc.Client.Cmdliner.standalone_cmd ~name:"rpc_client" () in
  exit @@ Cmdliner.Cmd.eval cmd
