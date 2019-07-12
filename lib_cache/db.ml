let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let no_callback _ = failwith "SQL query requested, but no callback supplied!"

let exec ?(cb=no_callback) stmt =
  let rec loop () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> ()
    | Sqlite3.Rc.ROW ->
      let cols = Sqlite3.data_count stmt in
      cb @@ List.init cols (fun i -> Sqlite3.column stmt i);
      loop ()
    | x -> Fmt.failwith "Sqlite3 exec error: %s" (Sqlite3.Rc.to_string x)
  in
  loop ()

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
    build : int64;
    value : string Current.or_error;
    rebuild : bool;
    finished : float;
  }

  let db = lazy (
    let db = Lazy.force Current.db in
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS build_cache ( \
                     builder   TEXT NOT NULL, \
                     key       BLOB, \
                     build     INTEGER NOT NULL, \
                     ok        BOOL NOT NULL, \
                     rebuild   BOOL NOT NULL DEFAULT 0, \
                     value     BLOB, \
                     job       TEXT NOT NULL, \
                     ready     DATETIME NOT NULL, \
                     running   DATETIME, \
                     finished  DATETIME NOT NULL, \
                     PRIMARY KEY (builder, key, build))" |> or_fail "create table";
    let record = Sqlite3.prepare db "INSERT INTO build_cache \
                                     (builder, key, build, ok, value, job, ready, running, finished) \
                                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" in
    let lookup = Sqlite3.prepare db "SELECT build, ok, rebuild, value, strftime('%s', finished) \
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
    Sqlite3.reset t.record |> or_fail "reset";
    let bind i v = Sqlite3.bind t.record i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT builder);
    bind 2 (Sqlite3.Data.BLOB key);
    bind 3 (Sqlite3.Data.INT build);
    begin
      match value with
      | Ok v ->
        bind 4 (Sqlite3.Data.INT 1L);
        bind 5 (Sqlite3.Data.BLOB v);
      | Error (`Msg v) ->
        bind 4 (Sqlite3.Data.INT 0L);
        bind 5 (Sqlite3.Data.BLOB v);
    end;
    bind 6 (Sqlite3.Data.TEXT job);
    bind 7 (Sqlite3.Data.TEXT (format_timestamp ready));
    bind 8
      (match running with
       | Some time -> Sqlite3.Data.TEXT (format_timestamp time);
       | None -> Sqlite3.Data.NULL
      );
    bind 9 (Sqlite3.Data.TEXT (format_timestamp finished));
    exec t.record

  let invalidate ~builder key =
    let t = Lazy.force db in
    Sqlite3.reset t.invalidate |> or_fail "reset";
    let bind i v = Sqlite3.bind t.invalidate i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT builder);
    bind 2 (Sqlite3.Data.BLOB key);
    exec t.invalidate

  let lookup ~builder key =
    let t = Lazy.force db in
    Sqlite3.reset t.lookup |> or_fail "reset";
    let bind i v = Sqlite3.bind t.lookup i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT builder);
    bind 2 (Sqlite3.Data.BLOB key);
    let result = ref None in
    let cb row =
      match !result with
      | Some _ -> Fmt.failwith "Multiple rows from lookup!"
      | None ->
        match row with
        | [ Sqlite3.Data.INT build;
            Sqlite3.Data.INT ok;
            Sqlite3.Data.INT rebuild;
            Sqlite3.Data.BLOB value;
            Sqlite3.Data.TEXT finished] ->
          let value = if ok = 1L then Ok value else Error (`Msg value) in
          let rebuild = rebuild <> 0L in
          let finished = float_of_string finished in
          result := Some { build; value; rebuild; finished }
        | _ -> Fmt.failwith "Invalid row from lookup!"
    in
    exec ~cb t.lookup;
    !result

  let drop_all builder =
    let t = Lazy.force db in
    Sqlite3.reset t.drop |> or_fail "reset";
    let bind i v = Sqlite3.bind t.drop i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT builder);
    exec t.drop
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
  }

  let db = lazy (
    let db = Lazy.force Current.db in
    Sqlite3.exec db "CREATE TABLE IF NOT EXISTS publish_cache ( \
                     op        TEXT NOT NULL, \
                     key       BLOB, \
                     job_id    TEXT NOT NULL, \
                     value     BLOB, \
                     PRIMARY KEY (op, key))" |> or_fail "create table";
    let record = Sqlite3.prepare db "INSERT OR REPLACE INTO publish_cache \
                                     (op, key, job_id, value) \
                                     VALUES (?, ?, ?, ?)" in
    let lookup = Sqlite3.prepare db "SELECT value, job_id FROM publish_cache WHERE op = ? AND key = ?" in
    let invalidate = Sqlite3.prepare db "DELETE FROM publish_cache WHERE op = ? AND key = ?" in
    let drop = Sqlite3.prepare db "DELETE FROM publish_cache WHERE op = ?" in
    { db; record; invalidate; drop; lookup }
  )

  let record ~op ~key ~job_id value =
    let t = Lazy.force db in
    Sqlite3.reset t.record |> or_fail "reset";
    let bind i v = Sqlite3.bind t.record i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT op);
    bind 2 (Sqlite3.Data.BLOB key);
    bind 3 (Sqlite3.Data.TEXT job_id);
    bind 4 (Sqlite3.Data.BLOB value);
    exec t.record

  let invalidate ~op key =
    let t = Lazy.force db in
    Sqlite3.reset t.invalidate |> or_fail "reset";
    let bind i v = Sqlite3.bind t.invalidate i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT op);
    bind 2 (Sqlite3.Data.BLOB key);
    exec t.invalidate

  let lookup ~op key =
    let t = Lazy.force db in
    Sqlite3.reset t.lookup |> or_fail "reset";
    let bind i v = Sqlite3.bind t.lookup i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT op);
    bind 2 (Sqlite3.Data.BLOB key);
    let result = ref None in
    let cb row =
      match !result with
      | Some _ -> Fmt.failwith "Multiple rows from lookup!"
      | None ->
        match row with
        | [Sqlite3.Data.BLOB value; TEXT job_id] -> result := Some { value; job_id }
        | _ -> Fmt.failwith "Invalid row from lookup!"
    in
    exec ~cb t.lookup;
    !result

  let drop_all op =
    let t = Lazy.force db in
    Sqlite3.reset t.drop |> or_fail "reset";
    let bind i v = Sqlite3.bind t.drop i v |> or_fail "bind" in
    bind 1 (Sqlite3.Data.TEXT op);
    exec t.drop
end
