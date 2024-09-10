type t = [
    `CheckRun
  | `CheckSuite
  | `PullRequest
  | `Push
  | `Create
  | `Installation
  | `InstallationRepositories
  ]

type checks_api_event = [ `Run | `Suite ]

let checks_api_event_to_string = function
  | `Run -> "check_run"
  | `Suite -> "check_suite"

let validate event =
  begin match event with
    | Some "installation_repositories" -> Ok `InstallationRepositories
    | Some "installation" -> Ok `Installation
    | Some "pull_request" -> Ok `PullRequest
    | Some "push" -> Ok `Push
    | Some "create" -> Ok `Create
    | Some "check_run" -> Ok `CheckRun
    | Some "check_suite" -> Ok `CheckSuite
    | Some x -> Fmt.error "Unknown GitHub event type %S" x
    | None -> Error "Missing GitHub event type in webhook!"
  end