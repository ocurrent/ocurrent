open Current.Syntax

module PC = Current_cache.Output(Post)

type channel = Post.t
let channel uri = uri

let post channel ~key message =
  "post" |>
  let** message = message in
  PC.set channel key message
