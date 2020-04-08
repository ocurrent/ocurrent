let src = Logs.Src.create "current_web" ~doc:"OCurrent web interface"
include (val Logs.src_log src : Logs.LOG)
