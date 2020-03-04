module M = Current.Log_matcher

let pp_rule f { M.pattern; report; score } =
  Fmt.pf f "%s/%s/%d" pattern report score

let rule = Alcotest.testable pp_rule (=)

let no_rule =
  let pp_no_rule f `Rule_not_found = Fmt.string f "Rule_not_found" in
  Alcotest.testable pp_no_rule (=)

let rule1 = { M.pattern = "Make exited with status (\\d+)"; report = "exit(\\1)"; score = 5 }
let rule2 = { M.pattern = "ENOSPACE"; report = "Out of disk space"; score = 10 }

let basic () =
  M.drop_all ();
  Alcotest.(check (option string)) "No rules" None @@ M.analyse_string "Make exited with status 2";
  M.add_rule rule1;
  Alcotest.(check (option string)) "One rule" (Some "exit(2)") @@ M.analyse_string "Make exited with status 2";
  M.add_rule rule2;
  Alcotest.(check (option string)) "Best first" (Some "Out of disk space") @@ M.analyse_string
    "Got ENOSPACE\n\
     Make exited with status 2";
  Alcotest.(check (option string)) "Best second" (Some "Out of disk space") @@ M.analyse_string
    "Make exited with status 2\n\
     Got ENOSPACE";
  M.add_rule { rule1 with score = 20 };
  Alcotest.(check (option string)) "Best now first" (Some "exit(2)") @@ M.analyse_string
    "Make exited with status 2\n\
     Got ENOSPACE";
  M.add_rule rule1;
  Alcotest.(check (list rule)) "List rules" [rule2; rule1] @@ M.list_rules ();
  Alcotest.(check (result unit no_rule)) "Remove rule" (Ok ()) @@ M.remove_rule rule2.M.pattern;
  Alcotest.(check (result unit no_rule)) "Remove rule twice" (Error `Rule_not_found) @@ M.remove_rule rule2.M.pattern;
  Alcotest.(check (option string)) "One again" (Some "exit(2)") @@ M.analyse_string
    "Make exited with status 2\n\
     Got ENOSPACE"

let tests =
  [
    Alcotest_lwt.test_case_sync "basic" `Quick basic;
  ]
