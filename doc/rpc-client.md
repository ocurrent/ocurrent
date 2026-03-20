# OCurrent RPC Client

This guide explains how to use the OCurrent RPC client to monitor and control pipelines remotely.

## Overview

OCurrent applications can expose an RPC endpoint that allows remote clients to:

- View pipeline statistics and state
- List and inspect active jobs
- Query job history from the database
- Cancel or rebuild jobs
- Get/set the confirmation level
- Generate pipeline visualizations (DOT graphs)

There are two ways to use the RPC client:

1. **Standalone client** - A separate `rpc_client` executable that works with any OCurrent application
2. **Embedded commands** - Add client subcommands directly to your application's executable

## Server Setup

To enable RPC on your OCurrent application, start it with `--capnp-address`:

```bash
base-images --capnp-address=tcp:127.0.0.1:9000
```

This creates a capability file (e.g., `/capnp-secrets/base-images.cap`) that clients use to connect. The capability file contains both the address and a secret token for authentication.

## Standalone Client

The standalone `rpc_client` works with any OCurrent application that exposes an RPC endpoint.

### Basic Usage

```bash
# Show pipeline overview (stats and current state)
rpc_client overview --connect=/capnp-secrets/base-images.cap

# List active jobs
rpc_client jobs -c /capnp-secrets/base-images.cap

# Get status of a specific job
rpc_client status -c /capnp-secrets/base-images.cap 2026-01-26/084338-git-repositories-b88382

# View job log
rpc_client log -c /capnp-secrets/base-images.cap 2026-01-26/084338-git-repositories-b88382
```

### Job Control

```bash
# Approve a job waiting for confirmation to start immediately
rpc_client start -c /capnp-secrets/base-images.cap 2026-01-26/084338-git-repositories-b88382

# Cancel a running job
rpc_client cancel -c /capnp-secrets/base-images.cap 2026-01-26/084338-ocluster-build-00b56d

# Rebuild a completed job
rpc_client rebuild -c /capnp-secrets/base-images.cap 2026-01-26/084338-git-repositories-b88382

# Rebuild multiple jobs at once
rpc_client rebuild-all -c /capnp-secrets/base-images.cap job1 job2 job3
```

### Querying Job History

The `query` command searches the job history database:

```bash
# List all historical jobs (most recent first, limit 100)
rpc_client query -c /capnp-secrets/base-images.cap

# Filter by date prefix
rpc_client query -c /capnp-secrets/base-images.cap --prefix=2026-01-26

# Filter by success/failure
rpc_client query -c /capnp-secrets/base-images.cap --ok=true   # successful jobs
rpc_client query -c /capnp-secrets/base-images.cap --ok=false  # failed jobs

# Filter by operation type
rpc_client query -c /capnp-secrets/base-images.cap --op=git-repositories

# Filter jobs needing rebuild
rpc_client query -c /capnp-secrets/base-images.cap --rebuild=true

# Combine filters
rpc_client query -c /capnp-secrets/base-images.cap --ok=false --prefix=2026-01
```

### Listing Operation Types

```bash
# List all operation types in the database
rpc_client ops -c /capnp-secrets/base-images.cap
```

### Configuration

```bash
# Get current confirmation level
rpc_client confirm -c /capnp-secrets/base-images.cap

# Set confirmation level
rpc_client confirm -c /capnp-secrets/base-images.cap --set=dangerous
rpc_client confirm -c /capnp-secrets/base-images.cap --set=harmless

# Disable confirmation requirement
rpc_client confirm -c /capnp-secrets/base-images.cap --set=none
```

Available confirmation levels (from least to most restrictive):
- `harmless`
- `mostly-harmless`
- `average`
- `above-average`
- `dangerous`

### Pipeline Visualization

```bash
# Output pipeline as DOT graph
rpc_client dot -c /capnp-secrets/base-images.cap > pipeline.dot

# Generate SVG visualization
rpc_client dot -c /capnp-secrets/base-images.cap | dot -Tsvg > pipeline.svg
```

## Embedded Client Commands

You can add RPC client commands directly to your application, allowing users to run commands like:

```bash
base-images client overview
base-images client jobs
base-images client query --ok=false
```

### Implementation

In your application's main module, use `Current_rpc.Client.Cmdliner.client_cmd`:

```ocaml
(* In base_images.ml *)

let client_cmd =
  Current_rpc.Client.Cmdliner.client_cmd
    ~name:"client"
    ~cap_file:"/capnp-secrets/base-images.cap"
    ()

let () =
  let main_cmd = (* your existing main command *) in
  let group = Cmdliner.Cmd.group main_info [main_cmd; client_cmd] in
  exit @@ Cmdliner.Cmd.eval group
```

The `~cap_file` parameter sets a default capability file, so users don't need to specify `--connect` for every command:

```bash
# These are equivalent when cap_file is set:
base-images client overview
base-images client overview --connect=/capnp-secrets/base-images.cap
```

Users can still override with `--connect` to connect to a different endpoint.

### Without Default Capability File

If you don't want a default, omit `~cap_file`:

```ocaml
let client_cmd =
  Current_rpc.Client.Cmdliner.client_cmd ~name:"client" ()
```

Users must then always specify `--connect`:

```bash
base-images client overview --connect=/path/to/engine.cap
```

## Example Session

Here's a typical workflow monitoring the base-images builder:

```bash
# Check overall pipeline health
$ rpc_client overview -c /capnp-secrets/base-images.cap
Pipeline Statistics:

OK:                       21
Waiting for confirmation: 55
Ready:                    0
Running:                  0
Failed:                   0
Blocked:                  1751

Total stages: 1827

Pipeline State: ACTIVE (waiting for confirmation)

# List active jobs
$ rpc_client jobs -c /capnp-secrets/base-images.cap
Active Jobs (53):
  2026-01-26/084338-git-repositories-b88382
  2026-01-26/084338-ocluster-build-00b56d
  ...

# Approve a job to start
$ rpc_client start -c /capnp-secrets/base-images.cap 2026-01-26/084338-git-repositories-b88382
Job approved to start.

# Check job history
$ rpc_client query -c /capnp-secrets/base-images.cap --ok=true
Job History (4 entries):

Job: 2026-01-26/084338-git-repositories-b88382 (build #1)
Outcome: OK: {"opam_repository_master":"f9f7db30..."}
Ready:    2026-01-26 08:43:38
  Started:  2026-01-26 08:48:36
Finished: 2026-01-26 08:50:35
Rebuild requested: false
...

# Check what operation types exist
$ rpc_client ops -c /capnp-secrets/base-images.cap
Operation Types:
  git-repositories

# Get current confirmation level
$ rpc_client confirm -c /capnp-secrets/base-images.cap
Confirmation level: harmless
```

## Server-Side Setup

For the RPC server to support all features including job history queries, ensure your application uses `Current_rpc.Impl`:

```ocaml
module Rpc = Current_rpc.Impl(Current)

let engine_capability = Rpc.engine my_engine
```

This automatically uses `Current_cache.Db` for job history queries, so the `query` and `ops` commands will return data from the SQLite database that OCurrent uses to track job history.
