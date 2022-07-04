(** The Github webhook-events that are supported *)
type t =
    [ `CheckRun
    | `CheckSuite
    | `Create
    | `Installation
    | `InstallationRepositories
    | `PullRequest
    | `Push ]

type checks_api_event = [ `Run | `Suite ]
(** events specific to the checks-api *)

val checks_api_event_to_string : checks_api_event -> string
(** convert checks_api_event variant into string representation *)

val validate : string option -> (t, string) result
(** convert string into supported webhook-event variant *)
