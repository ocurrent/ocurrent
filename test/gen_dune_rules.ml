let is_testcase name =
  Fpath.(has_ext "dot" (v name))

let pp_targets ppf filenames =
  Fmt.pf ppf
    {|(@[<v>rule@
(targets %a)
 (action (run ./test.exe)@]))

|}
    Fmt.(vbox (list ~sep:cut string)) filenames

let pp_runtest ppf filename =
  Fmt.pf ppf
    {|(alias
 (name runtest)
 (package current)
 (action (diff expected/%s %s)))
|}
    filename filename

let pp_duneinc =
  Fmt.(pp_targets ++ list ~sep:cut pp_runtest)

let () =
  Sys.readdir "expected"
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_testcase
  |> Fmt.(pp_duneinc ++ flush) Fmt.stdout
