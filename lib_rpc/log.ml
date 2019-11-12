let src = Logs.Src.create "current.rpc" ~doc:"OCurrent RPC"
include (val Logs.src_log src : Logs.LOG)
