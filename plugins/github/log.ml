let src = Logs.Src.create "current.github" ~doc:"OCurrent GitHub plugin"
include (val Logs.src_log src : Logs.LOG)
