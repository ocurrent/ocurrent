open Current.Syntax

module Config = Sandmark.Config

module SC = Current_cache.Make(Sandmark)

let sandmark ~pool ~config src =
  Current.component "sandmark" |>
  let> src = src in
  SC.get pool (config, src)

module RC = Current_cache.Make(Recent_commits)

let recent_commits ~n head =
  let+ digests =
    Current.component "last %d" n |>
    let> head = head in
    RC.get Recent_commits.No_context { Recent_commits.Key.head; n }
  and+ head = head in
  List.map (Current_git.Commit.ancestor head) digests
