open Current.Syntax

module R = Current_cache.Output(Run)

let run ~sw ~schedule ~key ~proc host args =
  Current.component "ssh@,%s" host |>
  let> args = args in
  R.set ~sw ~schedule (host, proc) key { Run.Value.args }
