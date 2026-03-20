@0xff960cc94b30cfb6;

# ============================================
# Existing types (unchanged)
# ============================================

struct JobStatus {
  id          @0 :Text;
  description @1 :Text;
  canCancel   @2 :Bool;
  canRebuild  @3 :Bool;
}

interface Job {
  status  @0 () -> JobStatus;
  cancel  @1 () -> ();
  rebuild @2 () -> (job :Job);

  log     @3 (start :Int64) -> (log :Data, next :Int64);
  # Return a chunk of log data starting at byte offset "start" and the
  # value to use for "start" in the next call to continue reading.
  # Returns 0 bytes of data to indicate end-of-file.
  # If the log is incomplete and there is no data currently available,
  # it waits until something changes before returning.
  # If "start" is negative then it is relative to the end of the log.

  approveEarlyStart @4 () -> ();
  # Mark the job as approved to start even if the global confirmation threshold
  # would otherwise prevent it. Calling this more than once has no effect.
}

# ============================================
# New types for extended functionality
# ============================================

struct PipelineStats {
  ok                     @0 :UInt32;
  waitingForConfirmation @1 :UInt32;
  ready                  @2 :UInt32;
  running                @3 :UInt32;
  failed                 @4 :UInt32;
  blocked                @5 :UInt32;
}

struct PipelineState {
  union {
    success @0 :Void;
    failed  @1 :Text;   # error message
    active  @2 :ActiveState;
  }
}

enum ActiveState {
  ready @0;
  running @1;
  waitingForConfirmation @2;
}

enum ConfirmLevel {
  harmless @0;
  mostlyHarmless @1;
  average @2;
  aboveAverage @3;
  dangerous @4;
}

struct JobHistoryEntry {
  jobId    @0 :Text;
  build    @1 :Int64;
  outcome  @2 :Outcome;
  ready    @3 :Float64;   # Unix timestamp
  running  @4 :Float64;   # 0.0 if never started
  finished @5 :Float64;
  rebuild  @6 :Bool;      # rebuild requested flag
}

struct Outcome {
  union {
    success @0 :Text;     # the cached value
    failure @1 :Text;     # error message
  }
}

struct OptBool {
  union {
    unset @0 :Void;
    value @1 :Bool;
  }
}

struct QueryParams {
  op        @0 :Text;     # empty = any
  ok        @1 :OptBool;
  rebuild   @2 :OptBool;
  jobPrefix @3 :Text;     # empty = any (typically YYYY-MM-DD)
}

struct RebuildResult {
  succeeded @0 :List(Text);
  failed    @1 :List(Text);
}

# ============================================
# Extended Engine interface
# ============================================

interface Engine {
  # Existing methods
  activeJobs @0 () -> (ids :List(Text));
  job        @1 (id :Text) -> (job :Job);

  # New: Query job history
  query      @2 (params :QueryParams) -> (entries :List(JobHistoryEntry));
  # Query the job history database with optional filters.
  # Returns matching job entries sorted by finished time (most recent first).

  ops        @3 () -> (ops :List(Text));
  # List all known operation types (e.g., "docker-build", "git-clone").

  # New: Pipeline overview
  pipelineStats @4 () -> (stats :PipelineStats);
  # Get counts of pipeline stages in each state.

  pipelineState @5 () -> (state :PipelineState);
  # Get the overall pipeline state (success, failed, or active).

  pipelineDot   @6 () -> (dot :Text);
  # Get the pipeline graph in Graphviz DOT format.

  # New: Configuration
  getConfirmLevel @7 () -> (level :ConfirmLevel, isSet :Bool);
  # Get the current confirmation level. If isSet is false, no confirmation is required.

  setConfirmLevel @8 (level :ConfirmLevel, unset :Bool) -> ();
  # Set the confirmation level. If unset is true, disables confirmation.

  # New: Bulk operations
  rebuildAll @9 (jobIds :List(Text)) -> RebuildResult;
  # Attempt to rebuild multiple jobs at once.
  # Returns lists of job IDs that succeeded or failed to be queued for rebuild.
}
