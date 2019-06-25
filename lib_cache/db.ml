type t = {
  db : Sqlite3.db;
  record : Sqlite3.stmt;
  lookup : Sqlite3.stmt;
}

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let db = lazy (
  let db = Lazy.force Current.db in
  Sqlite3.exec db "CREATE TABLE IF NOT EXISTS build_cache ( \
                   builder TEXT, \
                   key     BLOB, \
                   ok      BOOL NOT NULL, \
                   value   BLOB, \
                   log     BLOB NOT NULL, \
                   date    DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP, \
                   PRIMARY KEY (builder, key))" |> or_fail "create table";
  let record = Sqlite3.prepare db "INSERT OR REPLACE INTO build_cache (builder, key, ok, value, log) \
                                   VALUES (?, ?, ?, ?, ?)" in
  let lookup = Sqlite3.prepare db "SELECT ok, value FROM build_cache WHERE builder = ? AND key = ?" in
  { db; record; lookup }
)

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

let record ~builder ~key ~log value =
  let t = Lazy.force db in
  Sqlite3.reset t.record |> or_fail "reset";
  let bind i v = Sqlite3.bind t.record i v |> or_fail "bind" in
  bind 1 (Sqlite3.Data.TEXT builder);
  bind 2 (Sqlite3.Data.BLOB key);
  begin
    match value with
    | Ok v ->
      bind 3 (Sqlite3.Data.INT 1L);
      bind 4 (Sqlite3.Data.BLOB v);
    | Error (`Msg v) ->
      bind 3 (Sqlite3.Data.INT 0L);
      bind 4 (Sqlite3.Data.BLOB v);
  end;
  bind 5 (Sqlite3.Data.BLOB log);
  exec t.record

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
      result := Some (
          match row with
          | [Sqlite3.Data.INT 0L; Sqlite3.Data.BLOB msg] -> Error (`Msg msg)
          | [Sqlite3.Data.INT 1L; Sqlite3.Data.BLOB value] -> Ok value
          | _ -> Fmt.failwith "Invalid row from lookup!"
        )
  in
  exec ~cb t.lookup;
  !result

let drop_all () =
  let t = Lazy.force db in
  Sqlite3.exec t.db "DELETE FROM build_cache" |> or_fail "drop_all"
