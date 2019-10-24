(** Helper types for the below API calls *)
type status = [ `Queued | `In_progress | `Completed ] [@@deriving yojson]

type conclusion = [ `Success | `Failure | `Neutral | `Cancelled | `Timed_out | `Action_required ] [@@deriving yojson]

type annotation = {
  path: string;
  start_lint: int;
  end_line: int option;
  start_column: int option;
  end_column: int option;
  annotation_level: [`Notice | `Warning | `Failure] option;
  message: string;
  title: string option;
  raw_details: string option;
} [@@deriving yojson]

type image = {
  alt: string;
  image_url: string;
  caption: string option;
} [@@deriving yojson]

type output = {
  title: string;
  summary: string;
  text: string;
  annotations: annotation list;
  images: image list;
} [@@deriving yojson]

type action = {
  label: string;
  description: string;
  identifier: string;
} [@@deriving yojson]

(** POST /repos/:owner/:repo/check-runs
    See https://developer.github.com/v3/checks/runs/. *)
type post_check_run = {
  name: string;
  head_sha: string;
  details_url: string option;
  external_id: string;
  status: status;
  started_at: string;
  conclusion: conclusion option;
  completed_at: string option;
  output: output;
  actions: action list;
} [@@deriving yojson]

(** POST /repos/:owner/:repo/check-runs/:check_run_id *)
type patch_check_run = {
  name: string;
  details_url: string;
  external_id: string;
  started_at: string;
  status: status;
  conclusion: conclusion;
  completed_at: string;
  output: output;
} [@@deriving yojson]

