let src = Logs.Src.create "current.sandmark" ~doc:"OCurrent sandmark plugin"
include (val Logs.src_log src : Logs.LOG)
