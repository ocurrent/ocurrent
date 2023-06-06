type channel

val channel : Uri.t -> channel
(** Make a channel using the endpoint URI from Slack (create a new app, then add
    a new webhook using the "Incoming Webhooks" page to get the URI).
    e.g. [channel @@ Uri.of_string "https://hooks.slack.com/services/..."] *)

val post : Eio.Net.t -> channel -> key:string -> string Current.t -> unit Current.t
(** [post channel ~key message] records that [key] is now set to [message], and
    posts [message] to [channel] if it has changed.
    e.g. [post to_dev ~key:"build-status" s] *)
