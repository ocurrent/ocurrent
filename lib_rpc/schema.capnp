@0xff960cc94b30cfb6;

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
}

interface Engine {
  activeJobs @0 () -> (ids :List(Text));
  job        @1 (id :Text) -> (job :Job);
}
