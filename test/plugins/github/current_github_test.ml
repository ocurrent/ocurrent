open Current_github.Api_checks

let in_progress_conclusion () =
  (* example taken from https://developer.github.com/v3/checks/runs/#example-of-in_progress-conclusion *)
  ({
    name = "mighty_readme";
    head_sha = "ce587453ced02b1526dfb4cb910479d431683101";
    status = `In_progress;
    external_id = "42";
    started_at = "2018-05-04T01:14:52Z";
    output = {
      title = "Mighty Readme report";
      summary = "";
      text = "";
      annotations = [];
      images = [];
    };
    details_url = None;
    conclusion = None;
    completed_at = None;
    actions = [];
  } : post_check_run)
  |> post_check_run_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> Alcotest.(check string) "generated format is as expected" {|{
  "name": "mighty_readme",
  "head_sha": "ce587453ced02b1526dfb4cb910479d431683101",
  "details_url": null,
  "external_id": "42",
  "status": [ "In_progress" ],
  "started_at": "2018-05-04T01:14:52Z",
  "conclusion": null,
  "completed_at": null,
  "output": {
    "title": "Mighty Readme report",
    "summary": "",
    "text": "",
    "annotations": [],
    "images": []
  },
  "actions": []
}|}


let tests = [
  Alcotest.test_case "in_progress conclusion" `Quick in_progress_conclusion;
]
