let src = Logs.Src.create "current.docker" ~doc:"OCurrent docker plugin"
include (val Logs.src_log src : Logs.LOG)
