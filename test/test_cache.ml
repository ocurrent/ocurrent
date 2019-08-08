open Current.Syntax
open Lwt.Infix

let () = Driver.init_logging ()

module Builds = Map.Make(String)

module Build = struct
  module Key = Current.String
  module Value = Current.String

  type waker = string Current.or_error Lwt.u

  type t = waker Builds.t ref

  let create () = ref Builds.empty

  let id = "test-build"

  let pp = Fmt.string

  let build ~switch:_ ~set_running t _job key =
    set_running ();
    if Builds.mem key !t then Fmt.failwith "Already building %s!" key;
    let finished, set_finished = Lwt.wait () in
    t := Builds.add key set_finished !t;
    finished

  let auto_cancel = true

  let level _ _ = Current.Level.Average
end

module BC = Current_cache.Make(Build)

let get ?schedule builds x =
  Current.component "get %s" x |>
  let> () = Current.return () in
  BC.get ?schedule builds x

let some = function
  | None -> assert false
  | Some x -> x

let date = function
  | None -> -1
  | Some x -> int_of_string x

let disk_cache () =
  let db = Lazy.force Current.Db.v in
  let results = ref [] in
  let cb (row:Sqlite3.row) _ =
    match row with
    | [| key; ok; value; ready; running; finished |] ->
      let value = some value in
      let value = match some ok with "1" -> Ok value | "0" -> Error value | _ -> assert false in
      let row = (some key, value, date ready, date running, date finished) in
      results := row :: !results
    | _ -> assert false
  in
  match Sqlite3.exec ~cb db "SELECT key, ok, value, strftime('%s', ready), strftime('%s', running), strftime('%s', finished)
                             FROM build_cache WHERE builder='test-build' AND rebuild = 0" with
  | Sqlite3.Rc.OK -> List.sort compare !results
  | x -> Fmt.failwith "disk_cache: %s" (Sqlite3.Rc.to_string x)

let entry =
  let pp f (key, value, a, b, c) =
    Fmt.pf f "%s => %a (%d %d %d)" key Fmt.(result ~ok:string ~error:string) value a b c
  in
  Alcotest.testable pp (=)
let database = Alcotest.(list entry)

module Clock = struct
  type t = {
    mutable now : float;
    cond : unit Lwt_condition.t;
  }

  let create () =
    let cond = Lwt_condition.create () in
    let t = {now = 0.0; cond} in
    Current_cache.timestamp := (fun () -> t.now);
    Current_cache.sleep := (fun d ->
        let end_time = d +. t.now in
        let rec aux () =
          Logs.info (fun f -> f "sleep checking if %.0f >= %.0f yet" t.now end_time);
          if t.now >= end_time then Lwt.return_unit
          else Lwt_condition.wait cond >>= aux
        in
        aux ()
      );
    t

  let set t now =
    t.now <- now;
    Lwt_condition.broadcast t.cond ()
end

let basic _switch () =
  let result = ref "none" in
  let pipeline builds () =
    let+ x = get builds "a" in
    result := x
  in
  BC.reset ();
  let clock = Clock.create () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  let builds = Build.create () in
  Driver.test ~name:"cache" (pipeline builds) @@ function
  | 1 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Clock.set clock 1.0;
    Lwt.wakeup b @@ Ok "done"
  | 2 -> 
    Alcotest.(check string) "Result correct" "done" !result;
    Alcotest.check database "Result stored" ["a", Ok "done", 0, 0, 1] @@ disk_cache ();
    Driver.rebuild "a";
  | 3 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Lwt.wakeup b @@ Ok "rebuild"
  | 4 ->
    Alcotest.(check string) "Rebuild result" "rebuild" !result;
    raise Exit
  | _ ->
    assert false

let expires _switch () =
  let result = ref "none" in
  let five_s = Current_cache.Schedule.v ~valid_for:(Duration.of_sec 5) () in
  let ten_s = Current_cache.Schedule.v ~valid_for:(Duration.of_sec 10) () in
  let pipeline builds () =
    let+ x = get ~schedule:ten_s builds "a"
    and+ y = get ~schedule:five_s builds "a"
    in
    result := Fmt.strf "%s,%s" x y
  in
  BC.reset ();
  let clock = Clock.create () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  let builds = Build.create () in
  Driver.test ~name:"cache" (pipeline builds) @@ function
  | 1 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Clock.set clock 1.0;
    Lwt.wakeup b @@ Ok "done"
  | 2 -> 
    Alcotest.check database "Result stored" ["a", Ok "done", 0, 0, 1] @@ disk_cache ();
    Alcotest.(check string) "Result correct" "done,done" !result;
    Clock.set clock 7.0
  | 3 ->
    raise Driver.Expect_skip
  | 4 ->
    let b = Builds.find "a" !builds in
    Clock.set clock 8.0;
    Alcotest.check database "Disk store empty again" [] @@ disk_cache ();
    Lwt.wakeup b @@ Ok "rebuild"
  | _ ->
    Alcotest.check database "Result stored" ["a", Ok "rebuild", 7, 7, 8] @@ disk_cache ();
    Alcotest.(check string) "Result correct" "rebuild,rebuild" !result;
    raise Exit

module Bool_var = Current.Var(struct type t = bool let pp = Fmt.bool let equal = (=) end)
let wanted = Bool_var.create ~name:"wanted" (Ok true)

let autocancel _switch () =
  let result = ref "none" in
  let builds = Build.create () in
  let pipeline () =
    let* wanted = Bool_var.get wanted in
    let+ r =
      if wanted then get builds "a"
      else Current.return "unwanted"
    in
    result := r
  in
  BC.reset ();
  let clock = Clock.create () in
  Alcotest.check database "Disk store initially empty" [] @@ disk_cache ();
  Driver.test ~name:"cache" pipeline @@ function
  | 1 ->
    Alcotest.(check string) "Initially pending" "none" !result;
    Bool_var.set wanted @@ Ok false
  | 2 ->
    Alcotest.(check string) "Not wanted" "unwanted" !result;
    Bool_var.set wanted @@ Ok true
  | 3 ->
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Clock.set clock 1.0;
    Lwt.wakeup b @@ Ok "old-build"
  | 4 ->
    Alcotest.(check string) "No update yet" "unwanted" !result;
    let b = Builds.find "a" !builds in
    builds := Builds.remove "a" !builds;
    Clock.set clock 2.0;
    Lwt.wakeup b @@ Ok "new-build"
  | 5 ->
    Alcotest.(check string) "Rebuild done" "new-build" !result;
    raise Exit
  | _ ->
    assert false

module Publish = struct
  module Key = Current.String
  module Value = Current.String
  module Outcome = Current.Unit

  type t = {
    mutable state : string;
    mutable next : string;
    mutable set_finished : unit Current.or_error Lwt.u option;
  }

  let id = "publish"

  let complete t v =
    match t.set_finished with
    | None -> failwith "Publish.complete: nothing in progress!"
    | Some set_finished ->
      t.set_finished <- None;
      if v = Ok () then t.state <- t.next;
      t.next <- "unset";
      Lwt.wakeup set_finished v

  let publish ~switch t _job key value =
    Logs.info (fun f -> f "test_cache.publish");
    assert (key = "foo");
    assert (t.set_finished = None);
    let finished, set_finished = Lwt.wait () in
    t.set_finished <- Some set_finished;
    t.state <- t.state ^ "-changing";
    t.next <- value;
    Current.Switch.add_hook_or_exec switch (function
        | Ok () -> Lwt.return_unit
        | Error (`Msg reason) as e ->
          Logs.info (fun f -> f "Cancelling: %s" reason);
          t.state <- "cancelled";
          complete t e;
          Lwt.return_unit
      )
    >>= fun () ->
    finished

  let pp f (k, v) =
    Fmt.pf f "Set %s to %s" k v

  let level _ _ _ = Current.Level.Average

  let auto_cancel = false

  let create () =
    { state = "init"; set_finished = None; next = "unset" }
end

module V = Current.Var(Current.String)

let input = V.create ~name:"input" @@ Ok "bar"

module OC = Current_cache.Output(Publish)

let set p k v =
  Current.component "set" |>
  let> v = v in
  OC.set p k v

let output _switch () =
  V.set input @@ Ok "bar";
  OC.reset ();
  let p = Publish.create () in
  let pipeline () = V.get input |> set p "foo" in
  Driver.test ~name:"cache.output" pipeline @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 2 ->
    Alcotest.(check string) "Publish has completed" "bar" p.Publish.state;
    V.set input @@ Ok "baz";
  | 3 ->
    Alcotest.(check string) "Changing to baz" "bar-changing" p.Publish.state;
    V.set input @@ Ok "new";
  | 4 ->
    Alcotest.(check string) "Changed during publish" "bar-changing" p.Publish.state;
    Publish.complete p @@ Error (`Msg "baz failed");
  | 5 ->
    Alcotest.(check string) "First change failed" "bar-changing-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 6 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    raise Exit
  | _ ->
    assert false

module Publish2 = struct
  include Publish
  let id = "publish2"
  let auto_cancel = true
end

module OC2 = Current_cache.Output(Publish2)

let set2 p k v =
  Current.component "set2" |>
  let> v = v in
  OC2.set p k v

let output_autocancel _switch () =
  V.set input @@ Ok "bar";
  OC2.reset ();
  let p = Publish2.create () in
  let pipeline () = V.get input |> set2 p "foo" in
  Driver.test ~name:"cache.output_autocancel" pipeline @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 2 ->
    Alcotest.(check string) "Publish has completed" "bar" p.Publish.state;
    V.set input @@ Ok "baz";
  | 3 ->
    Alcotest.(check string) "Changing to baz" "bar-changing" p.Publish.state;
    V.set input @@ Ok "new";
  | 4 ->
    Alcotest.(check string) "Changed during publish" "cancelled-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 5 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    Driver.rebuild "Set foo to new (completed)";
  | 6 ->
    Alcotest.(check string) "Re-publish has started" "new-changing" p.Publish.state;
    Publish.complete p @@ Ok ();
  | 7 ->
    Alcotest.(check string) "Success" "new" p.Publish.state;
    raise Exit
  | _ ->
    assert false

let output_retry _switch () =
  OC2.reset ();
  let p = Publish2.create () in
  let pipeline () = set2 p "foo" (Current.return "value") in
  Driver.test ~name:"cache.output_retry" pipeline @@ function
  | 1 ->
    Alcotest.(check string) "Publish has started" "init-changing" p.Publish.state;
    Publish.complete p @@ Error (`Msg "Failed")
  | 2 ->
    Driver.rebuild "Set foo to value: Failed"
  | 3 ->
    Alcotest.(check string) "Publish has restarted" "init-changing-changing" p.Publish.state;
    Publish.complete p @@ Ok ()
  | 4 ->
    Alcotest.(check string) "Publish has completed" "value" p.Publish.state;
    raise Exit
  | _ ->
    assert false

let tests =
  [
    Driver.test_case_gc "basic"             basic;
    Driver.test_case_gc "expires"           expires;
    Driver.test_case_gc "autocancel"        autocancel;
    Driver.test_case_gc "output"            output;
    Driver.test_case_gc "output_autocancel" output_autocancel;
    Driver.test_case_gc "output_retry"      output_retry;
  ]
