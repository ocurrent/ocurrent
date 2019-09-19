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
  rebuild @2 () -> ();
  log     @3 () -> (log :Text);
}

interface Engine {
  activeJobs @0 () -> (ids :List(Text));
  job        @1 (id :Text) -> (job :Job);
}
