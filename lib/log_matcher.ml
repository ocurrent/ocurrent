type rule = {
  pattern : string;
  report : string;
  score : int;
}

module Dao = struct
  type t = {
    db : Sqlite3.db;
    load : Sqlite3.stmt;
    add : Sqlite3.stmt;
    remove : Sqlite3.stmt;
    delete_all : Sqlite3.stmt;
  }

  let v = lazy (
    let db = Lazy.force Db.v in
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS log_regexp ( \
                     re       TEXT NOT NULL, \
                     report   TEXT NOT NULL, \
                     score    INTEGER NOT NULL, \
                     PRIMARY KEY (re))" |> Db.or_fail ~cmd:"create log_regexp";
    let load = Sqlite3.prepare db "SELECT re, report, score FROM log_regexp ORDER BY re" in
    let add = Sqlite3.prepare db "INSERT OR REPLACE INTO log_regexp (re, report, score) VALUES (?, ?, ?)" in
    let remove = Sqlite3.prepare db "DELETE FROM log_regexp WHERE re = ?" in
    let delete_all = Sqlite3.prepare db "DELETE FROM log_regexp" in
    { db; load; add; remove; delete_all }
  )

  let load () =
    let t = Lazy.force v in
    Db.query t.load [] |> List.map @@ function
    | Sqlite3.Data.[ TEXT pattern; TEXT report; INT score ] -> { pattern; report; score = Int64.to_int score }
    | row -> Fmt.failwith "load: invalid row %a" Db.dump_row row

  let add { pattern; report; score } =
    let t = Lazy.force v in
    Db.exec t.add Sqlite3.Data.[ TEXT pattern; TEXT report; INT (Int64.of_int score) ]

  let remove pattern =
    let t = Lazy.force v in
    Db.exec t.remove Sqlite3.Data.[ TEXT pattern ];
    match Sqlite3.changes t.db with
    | 0 -> Error `Rule_not_found
    | 1 -> Ok ()
    | x -> Fmt.failwith "Multiple rows updated (%d)!" x

  let drop_all () =
    let t = Lazy.force v in
    Db.exec t.delete_all []
end

type test = {
  re : Re.t;
  report : string;
  score : int;
  mark :Re.Mark.t;
}

type t = {
  tests : test list;
  combined : Re.re;
}

let re_subst =
  let (++) a b = Re.seq [a; b] in
  Re.(compile @@ char '\\' ++ group (rep1 digit))

let t = ref None

let get () =
  match !t with
  | Some t -> t
  | None ->
    let raw = Dao.load () in
    let tests =
      raw |> List.filter_map (fun {pattern; report; score} ->
          try
            let mark, re = Re.Pcre.re pattern |> Re.mark in
            Some { re; report; mark; score }
          with ex ->
            Log.err (fun f -> f "Invalid pattern %S: %a" pattern Fmt.exn ex);
            None
        )
    in
    let combined = Re.no_group (Re.alt (List.map (fun x -> x.re) tests)) |> Re.compile in
    let v = { tests; combined } in
    t := Some v;
    v

let analyse_string ?job log_text =
  let t = get () in
  let did_header = ref false in
  Re.Seq.all t.combined log_text
  |> Seq.fold_left (fun best group ->
      (* For each match, find out which test matched. *)
      let matched = Re.Group.get group 0 in
      let test = List.find (fun test -> Re.Mark.test group test.mark) t.tests in
      job |> Option.iter (fun job ->
          if not !did_header then (
            Job.log job "Log analysis:";
            did_header := true
          );
          Job.log job ">>> %s (score = %d)" matched test.score);
      match best with
      | Some (best_score, _, _) when best_score >= test.score -> best
      | _ -> Some (test.score, group, test)
    ) None
  |> function
  | None -> None      (* No matches found *)
  | Some (_score, group, test) ->
    (* [test] is the best match. Run it again with matching to generate the report. *)
    let pos, stop = Re.Group.offset group 0 in
    let group = Re.exec ~pos ~len:(stop - pos) (Re.compile test.re) log_text in
    let subst g =
      let r = Re.Group.get g 1 in
      try Re.Group.get group (int_of_string r)
      with ex ->
        Log.err (fun f -> f "Bad group %S in report %S: %a" r test.report Fmt.exn ex);
        Fmt.str "Bad group %S in report %S: %a" r test.report Fmt.exn ex
    in
    let report = Re.replace ~all:true re_subst ~f:subst test.report in
    job |> Option.iter (fun job -> Job.log job "%s" report);
    Some report

let analyse_file ?job log_path =
  let ch = open_in_bin (Fpath.to_string log_path) in
  (* re doesn't support streaming, so load the whole log at once. *)
  let log_text =
    Fun.protect
      (fun () -> really_input_string ch (in_channel_length ch))
      ~finally:(fun () -> close_in ch)
  in
  analyse_string ?job log_text

let analyse_job job =
  match Job.log_path (Job.id job) with
  | Error `Msg e -> Fmt.failwith "Job log missing! %s" e
  | Ok log_path -> analyse_file ~job log_path

let add_rule rule =
  ignore (Re.Pcre.re rule.pattern);  (* Check it compiles *)
  Dao.add rule;
  t := None

let remove_rule pattern =
  Dao.remove pattern |> Stdlib.Result.map (fun () -> t := None)

let list_rules () =
  Dao.load ()

let drop_all () =
  Dao.drop_all ();
  t := None
