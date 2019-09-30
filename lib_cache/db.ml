module Db = Current.Db

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let format_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.strf "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

type t = {
  db : Sqlite3.db;
  record : Sqlite3.stmt;
  invalidate : Sqlite3.stmt;
  drop : Sqlite3.stmt;
  lookup : Sqlite3.stmt;
}

type entry = {
  job_id : string;
  build : int64;
  value : string;
  outcome : string Current.or_error;
  ready : float;
  running : float option;
  finished : float;
  rebuild : bool;
}

let db = lazy (
  let db = Lazy.force Current.Db.v in
  Sqlite3.exec db "CREATE TABLE IF NOT EXISTS cache ( \
                   op        TEXT NOT NULL, \
                   key       BLOB, \
                   job_id    TEXT NOT NULL, \
                   value     BLOB, \
                   ok        BOOL NOT NULL, \
                   outcome   BLOB, \
                   build     INTEGER NOT NULL, \
                   rebuild   BOOL NOT NULL DEFAULT 0, \
                   ready     DATETIME NOT NULL, \
                   running   DATETIME, \
                   finished  DATETIME NOT NULL, \
                   PRIMARY KEY (op, key, build))" |> or_fail "create table";
  let record = Sqlite3.prepare db "INSERT OR REPLACE INTO cache \
                                   (op, key, job_id, value, ok, outcome, ready, running, finished, build) \
                                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" in
  let lookup = Sqlite3.prepare db "SELECT value, job_id, ok, outcome, \
                                          strftime('%s', ready), \
                                          strftime('%s', running), \
                                          strftime('%s', finished), \
                                          rebuild, build \
                                   FROM cache WHERE op = ? AND key = ? \
                                   ORDER BY build DESC \
                                   LIMIT 1" in
  let invalidate = Sqlite3.prepare db "UPDATE cache SET rebuild = 1 WHERE op = ? AND key = ?" in
  let drop = Sqlite3.prepare db "DELETE FROM cache WHERE op = ?" in
  { db; record; invalidate; drop; lookup }
)

let record ~op ~key ~value ~job_id ~ready ~running ~finished ~build outcome =
  let ok, outcome =
    match outcome with
    | Ok x -> 1L, x
    | Error (`Msg m) -> 0L, m
  in
  let t = Lazy.force db in
  let running = match running with
    | Some time -> Sqlite3.Data.TEXT (format_timestamp time);
    | None -> Sqlite3.Data.NULL
  in
  Db.exec t.record Sqlite3.Data.[ TEXT op; BLOB key; TEXT job_id; BLOB value;
                                  INT ok; BLOB outcome;
                                  TEXT (format_timestamp ready);
                                  running;
                                  TEXT (format_timestamp finished);
                                  INT build;
                                ]

let invalidate ~op key =
  let t = Lazy.force db in
  Db.exec t.invalidate Sqlite3.Data.[ TEXT op; BLOB key ]

let lookup ~op key =
  let t = Lazy.force db in
  match Db.query_some t.lookup Sqlite3.Data.[ TEXT op; BLOB key ] with
  | None -> None
  | Some Sqlite3.Data.[ BLOB value; TEXT job_id; INT ok; BLOB outcome;
                        TEXT ready; running; TEXT finished;
                        INT rebuild; INT build ] ->
    let ready = float_of_string ready in
    let running =
      match running with
      | Sqlite3.Data.TEXT running -> Some (float_of_string running)
      | NULL -> None
      | _ -> assert false
    in
    let finished = float_of_string finished in
    let outcome = if ok = 1L then Ok outcome else Error (`Msg outcome) in
    let rebuild = rebuild = 1L in
    Some { value; job_id; outcome; ready; running; finished; rebuild; build }
  | Some _ -> Fmt.failwith "Invalid row from lookup!"

let drop_all op =
  let t = Lazy.force db in
  Db.exec t.drop Sqlite3.Data.[ TEXT op ]

let finalize stmt () =
  let _ : Sqlite3.Rc.t = Sqlite3.finalize stmt in
  ()

let pp_where_clause f = function
  | [] -> ()
  | tests -> Fmt.pf f "WHERE %a" Fmt.(list ~sep:(unit " AND ") string) tests

let sqlite_bool = function
  | false -> Sqlite3.Data.INT 0L
  | true -> Sqlite3.Data.INT 1L

let query ?op ?ok ?rebuild () =
  let tests = List.filter_map Fun.id [
      Option.map (fun x -> Fmt.strf "ok=?", sqlite_bool x) ok;
      Option.map (fun x -> Fmt.strf "op=?", Sqlite3.Data.TEXT x) op;
      Option.map (fun x -> Fmt.strf "rebuild=?", sqlite_bool x) rebuild;
  ] in
  let t = Lazy.force db in
  let query = Sqlite3.prepare t.db (
      Fmt.strf "SELECT build, ok, outcome, rebuild, value,
                strftime('%%s', ready),
                strftime('%%s', running),
                strftime('%%s', finished),
                job_id \
                FROM cache \
                %a \
                ORDER BY finished DESC \
                LIMIT 100"
        pp_where_clause (List.map fst tests)
    )
  in
  Fun.protect ~finally:(finalize query) @@ fun () ->
  Db.query query (List.map snd tests)
  |> List.map @@ function
  | Sqlite3.Data.[INT build; INT ok; BLOB outcome; INT rebuild; BLOB value; TEXT ready; running; TEXT finished; TEXT job_id] ->
    let outcome = if ok = 1L then Ok outcome else Error (`Msg outcome) in
    let rebuild = rebuild <> 0L in
    let ready = float_of_string ready in
    let running =
      match running with
      | Sqlite3.Data.TEXT running -> Some (float_of_string running)
      | NULL -> None
      | _ -> assert false
    in
    let finished = float_of_string finished in
    { build; value; rebuild; ready; running; finished; job_id; outcome }
  | _ -> Fmt.failwith "Invalid row from query!"
