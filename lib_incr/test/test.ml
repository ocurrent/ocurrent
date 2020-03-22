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

let () =
  Alcotest.run "incr" [
    "basic", [
      Alcotest.test_case  "simple"  `Quick test_simple;
      Alcotest.test_case  "release" `Quick test_release;
      Alcotest.test_case  "eq"      `Quick test_eq;
      Alcotest.test_case  "expand"  `Quick test_expand;
      Alcotest.test_case  "nested"  `Quick test_nested;
      Alcotest.test_case  "leak"    `Quick test_leak;
    ]
  ]
