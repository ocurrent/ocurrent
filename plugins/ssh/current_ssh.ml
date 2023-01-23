open Current.Syntax

module R = Current_cache.Output(Run)

let run ~schedule ~key host args =
  Current.component "ssh@,%s" host |>
  let> args = args in
  R.set ~schedule host key { Run.Value.args }
