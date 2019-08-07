module Db = Current.Db

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let format_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.strf "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

module Build = struct
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
    value : string Current.or_error;
    rebuild : bool;
    finished : float;
  }

  let db = lazy (
    let db = Lazy.force Db.v in
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS build_cache ( \
                     builder   TEXT NOT NULL, \
                     key       BLOB, \
                     build     INTEGER NOT NULL, \
                     ok        BOOL NOT NULL, \
                     rebuild   BOOL NOT NULL DEFAULT 0, \
                     value     BLOB, \
                     job_id    TEXT NOT NULL, \
                     ready     DATETIME NOT NULL, \
                     running   DATETIME, \
                     finished  DATETIME NOT NULL, \
                     PRIMARY KEY (builder, key, build))" |> or_fail "create table";
    let record = Sqlite3.prepare db "INSERT INTO build_cache \
                                     (builder, key, build, ok, value, job_id, ready, running, finished) \
                                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" in
    let lookup = Sqlite3.prepare db "SELECT build, ok, rebuild, value, strftime('%s', finished), job_id \
                                     FROM build_cache \
                                     WHERE builder = ? AND key = ? \
                                     ORDER BY build DESC \
                                     LIMIT 1" in
    let invalidate = Sqlite3.prepare db "UPDATE build_cache SET rebuild = 1 WHERE builder = ? AND key = ?" in
    let drop = Sqlite3.prepare db "DELETE FROM build_cache WHERE builder = ?" in
    { db; record; invalidate; drop; lookup }
  )

  let record ~builder ~build ~key ~job ~ready ~running ~finished value =
    let t = Lazy.force db in
    let ok, value =
      match value with
      | Ok v -> 1L, v
      | Error (`Msg v) -> 0L, v
    in
    Db.exec t.record Sqlite3.Data.[
      TEXT builder;
      BLOB key;
      INT build;
      INT ok;
      BLOB value;
      TEXT job;
      TEXT (format_timestamp ready);
      (match running with
       | Some time -> TEXT (format_timestamp time);
       | None -> NULL
      );
      TEXT (format_timestamp finished);
    ]

  let invalidate ~builder key =
    let t = Lazy.force db in
    Db.exec t.invalidate Sqlite3.Data.[ TEXT builder; BLOB key ]

  let lookup ~builder key =
    let t = Lazy.force db in
    Sqlite3.reset t.lookup |> or_fail "reset";
    match Db.query_some t.lookup Sqlite3.Data.[ TEXT builder; BLOB key ] with
    | None -> None
    | Some Sqlite3.Data.[INT build; INT ok; INT rebuild; BLOB value; TEXT finished; TEXT job_id] ->
      let value = if ok = 1L then Ok value else Error (`Msg value) in
      let rebuild = rebuild <> 0L in
      let finished = float_of_string finished in
      Some { build; value; rebuild; finished; job_id }
    | Some _ -> Fmt.failwith "Invalid row from lookup!"

  let drop_all builder =
    let t = Lazy.force db in
    Db.exec t.drop Sqlite3.Data.[TEXT builder]
end

module Publish = struct
  type t = {
    db : Sqlite3.db;
    record : Sqlite3.stmt;
    invalidate : Sqlite3.stmt;
    drop : Sqlite3.stmt;
    lookup : Sqlite3.stmt;
  }

  type entry = {
    job_id : string;
    value : string;
    outcome : string;
  }

  let db = lazy (
    let db = Lazy.force Current.Db.v in
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS publish_cache ( \
                     op        TEXT NOT NULL, \
                     key       BLOB, \
                     job_id    TEXT NOT NULL, \
                     value     BLOB, \
                     outcome   BLOB, \
                     PRIMARY KEY (op, key))" |> or_fail "create table";
    let record = Sqlite3.prepare db "INSERT OR REPLACE INTO publish_cache \
                                     (op, key, job_id, value, outcome) \
                                     VALUES (?, ?, ?, ?, ?)" in
    let lookup = Sqlite3.prepare db "SELECT value, job_id, outcome FROM publish_cache WHERE op = ? AND key = ?" in
    let invalidate = Sqlite3.prepare db "DELETE FROM publish_cache WHERE op = ? AND key = ?" in
    let drop = Sqlite3.prepare db "DELETE FROM publish_cache WHERE op = ?" in
    { db; record; invalidate; drop; lookup }
  )

  let record ~op ~key ~value ~job_id outcome =
    let t = Lazy.force db in
    Db.exec t.record Sqlite3.Data.[ TEXT op; BLOB key; TEXT job_id; BLOB value; BLOB outcome ]

  let invalidate ~op key =
    let t = Lazy.force db in
    Db.exec t.invalidate Sqlite3.Data.[ TEXT op; BLOB key ]

  let lookup ~op key =
    let t = Lazy.force db in
    match Db.query_some t.lookup Sqlite3.Data.[ TEXT op; BLOB key ] with
    | None -> None
    | Some Sqlite3.Data.[ BLOB value; TEXT job_id; BLOB outcome ] -> Some { value; job_id; outcome }
    | Some _ -> Fmt.failwith "Invalid row from lookup!"

  let drop_all op =
    let t = Lazy.force db in
    Db.exec t.drop Sqlite3.Data.[ TEXT op ]
end
