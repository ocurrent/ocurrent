open Current_incr

module Msg = Set.Make(String)

let log = ref Msg.empty

let reset_log () =
  log := Msg.empty

let printf fmt =
  fmt |> Format.kasprintf @@ fun msg ->
  log := !log |> Msg.add msg;
  on_release (fun () -> log := !log |> Msg.remove msg)

(* [y = x + 1]. Check [y] updates when [x] does. *)
let test_simple () =
  let x = var 0 in
  let y =
    of_cc begin
      read (of_var x) @@ fun x ->
      write (x + 1)
    end
  in
  Alcotest.(check int) "Initial run" 1 @@ observe y;
  change x 3;
  propagate ();
  Alcotest.(check int) "Updated run" 4 @@ observe y

(* Check we can undo a (logging) side-effect when recomputing. *)
let test_release () =
  reset_log ();
  let x = var 0 in
  let y =
    of_cc begin
      read (of_var x) @@ fun x ->
      printf "Got x = %d" x;
      write (x + 1)
    end
  in
  Alcotest.(check int) "Initial run" 1 @@ observe y;
  Alcotest.(check (list string)) "Log run" ["Got x = 0"] @@ Msg.elements !log;
  change x 3;
  propagate ();
  Alcotest.(check int) "Updated run" 4 @@ observe y;
  Alcotest.(check (list string)) "Log run" ["Got x = 3"] @@ Msg.elements !log

(* Check that we stop recomputing when an intermediate value turns out to be the same. *)
let test_eq () =
  let i = ref 0 in
  let x = var 0 in
  let y =
    of_cc begin
      read (of_var x) @@ fun x ->
      incr i;
      if x > 0 then write 1
      else write 2
    end
  in
  let z =
    of_cc begin
      read y @@ fun x ->
      incr i;
      write (x * 10)
    end
  in
  Alcotest.(check int) "Initial run" 20 @@ observe z;
  Alcotest.(check int) "Two evals" 2 !i;
  change x (-1);
  propagate ();
  Alcotest.(check int) "Updated run" 20 @@ observe z;
  Alcotest.(check int) "One more eval" 3 !i;
  change ~eq:(=) x (-1);
  propagate ();
  Alcotest.(check int) "No more eval" 3 !i

(* Check we can cope with the second evaluation creating extra time-points. *)
let test_expand () =
  let x = var 0 in
  let y = var 10 in
  let z =
    of_cc begin
      read (of_var x) @@ fun x ->
      if x > 0 then (
        read (of_var y) @@ fun y ->
        write y
      ) else write 0
    end
  in
  Alcotest.(check int) "Initial run" 0 @@ observe z;
  change x 5;
  propagate ();
  Alcotest.(check int) "Now using y" 10 @@ observe z;
  change y 7;
  propagate ();
  Alcotest.(check int) "New y" 7 @@ observe z

(* Check that we erase nested functions when their parent is recalculated. *)
let test_nested () =
  let i = ref 0 in
  let x = var 1 in
  let z =
    of_cc begin
      read (of_var x) @@ fun v1 ->
      read (of_var x) @@ fun v2 ->
      incr i;
      write (v1 + v2)
    end
  in
  Alcotest.(check int) "Initial run" 2 @@ observe z;
  Alcotest.(check int) "One eval" 1 @@ !i;
  change x 2;
  propagate ();
  Alcotest.(check int) "Now double" 4 @@ observe z;
  Alcotest.(check int) "Only one extra eval" 2 @@ !i

let heap () =
  Gc.full_major ();
  Gc.((stat ()).live_words)

(* We keep reading from [y], which never changes. Check we don't leak memory. *)
let test_leak () =
  let x = var 0 in
  let y = var 1 in
  let z = of_cc begin
      read (of_var x) @@ fun x ->
      read (of_var y) @@ fun y ->
      write (x + y)
    end
  in
  Alcotest.(check int) "Initial run" 1 @@ observe z;
  let h0 = heap () in
  for i = 1 to 100 do
    change x i;
    propagate ();
  done;
  let h1 = heap () in
  Alcotest.(check int) "Memory usage constant" 0 @@ h1 - h0;
  change x 0    (* Stop [x] from being GC'd before here *)

module String_map = Map.Make(String)
module Separate_strings = Current_incr.Separate(String_map)

(* Check we can process individual set elements separately. *)
let test_separate () =
  log := Msg.empty;
  let active = var true in
  let count_each = ref 0 in
  let count_output_updates = ref 0 in
  let suffix = var "1" in
  let x = var (String_map.singleton "a" ()) in
  let z =
      of_cc begin
        read (of_var active) @@ function
        | false -> write "disabled"
        | true ->
          let y =
            (* Process each element of [x] individually. *)
            Separate_strings.map (of_var x) @@ fun x ->
            printf "process %s" x;
            if x = "b" then (
              read (of_var suffix) @@ fun suffix ->
              incr count_each;
              write ~eq:String.equal (x ^ suffix)
            ) else (
              incr count_each;
              write ~eq:String.equal (x ^ "-fixed")
            )
          in
          (* Combine the results *)
          read y @@ fun results ->
          incr count_output_updates;
          results
          |> String_map.bindings
          |> List.map (fun (k, v) -> Printf.sprintf "%s.%s" k v)
          |> String.concat ","
          |> write
      end
  in
  let update f =
    Current_incr.observe (Current_incr.of_var x) |> f |> Current_incr.change x;
    Current_incr.propagate ()
  in
  Alcotest.(check string) "Initial value" "a.a-fixed" @@ observe z;
  Alcotest.(check int) "One eval" 1 !count_each;
  Alcotest.(check int) "One output" 1 !count_output_updates;
  (* Add b - only runs [b] step. *)
  update (String_map.add "b" ());
  Alcotest.(check string) "Add b" "a.a-fixed,b.b1" @@ observe z;
  Alcotest.(check int) "One more eval" 2 !count_each;
  Alcotest.(check int) "Another output" 2 !count_output_updates;
  (* Change suffix. Only runs for [b]. *)
  change suffix "2";
  propagate ();
  Alcotest.(check string) "Change suffix" "a.a-fixed,b.b2" @@ observe z;
  Alcotest.(check int) "One more eval" 3 !count_each;
  Alcotest.(check int) "Another output" 3 !count_output_updates;
  (* Remove add. Nothing needs to run. *)
  update (String_map.remove "a");
  Alcotest.(check string) "Remove a" "b.b2" @@ observe z;
  Alcotest.(check int) "No more evals" 3 !count_each;
  Alcotest.(check int) "Another output" 4 !count_output_updates;
  (* Push a change that doesn't affect the final output. *)
  change suffix "2" ~eq:(fun _ _ -> false);
  propagate ();
  Alcotest.(check int) "One more eval" 4 !count_each;
  Alcotest.(check int) "No output change" 4 !count_output_updates;
  Alcotest.(check (list string)) "Check logs" ["process b"] @@ Msg.elements !log;
  (* Disable the whole thing. *)
  change active false;
  propagate ();
  Alcotest.(check (list string)) "Check logs" [] @@ Msg.elements !log;
  ()

let () =
  Alcotest.run "incr" [
    "basic", [
      Alcotest.test_case  "simple"   `Quick test_simple;
      Alcotest.test_case  "release"  `Quick test_release;
      Alcotest.test_case  "eq"       `Quick test_eq;
      Alcotest.test_case  "expand"   `Quick test_expand;
      Alcotest.test_case  "nested"   `Quick test_nested;
      Alcotest.test_case  "leak"     `Quick test_leak;
      Alcotest.test_case  "separate" `Quick test_separate;
    ]
  ]
