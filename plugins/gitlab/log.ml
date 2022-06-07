let src = Logs.Src.create "current.gitlab" ~doc:"OCurrent GitLab plugin"
include (val Logs.src_log src : Logs.LOG)
