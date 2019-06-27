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

  let build ~switch:_ t _job key =
    if Builds.mem key !t then Fmt.failwith "Already building %s!" key;
    let finished, set_finished = Lwt.wait () in
    t := Builds.add key set_finished !t;
    finished

  let auto_cancel = true

  let level _ _ = Current.Level.Average
end

module BC = Current_cache.Make(Build)

let get ?schedule builds x =
  Fmt.strf "get %s" x |>
  let** () = Current.return () in
  BC.get ?schedule builds x

let some = function
  | None -> assert false
  | Some x -> x

let date = function
  | None -> -1
  | Some x -> int_of_string x

let disk_cache () =
  let db = Lazy.force Current.db in
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
                             FROM build_cache WHERE builder='test-build'" with
  | Sqlite3.Rc.OK -> List.sort compare !results
  | x -> failwith (Sqlite3.Rc.to_string x)

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
    Clock.set clock 1.0;
    Lwt.wakeup b @@ Ok "done"
  | 2 -> 
    Alcotest.(check string) "Result correct" "done" !result;
    Alcotest.check database "Result stored" ["a", Ok "done", 0, 0, 1] @@ disk_cache ();
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

let tests =
  [
    Alcotest_lwt.test_case "basic" `Quick basic;
    Alcotest_lwt.test_case "expires" `Quick expires;
    Alcotest_lwt.test_case "autocancel" `Quick autocancel;
  ]
