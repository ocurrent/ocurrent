open Current.Syntax

module PC = Current_cache.Output(Post)

type channel = Post.channel
let channel uri = uri

let post net channel ~key message =
  Current.component "post" |>
  let> message = message in
  PC.set (channel, net) key message
