let state_dir_root = Fpath.v @@ Filename.concat (Sys.getcwd ()) "var"

let state_dir name =
  let name = Fpath.v name in
  assert (Fpath.is_rel name);
  let path = Fpath.append state_dir_root name in
  match Bos.OS.Dir.create path with
  | Ok (_ : bool) -> path
  | Error (`Msg m) -> failwith m

let db =
  let or_fail label x =
    match x with
    | Sqlite3.Rc.OK -> ()
    | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)
  in
  lazy (
    let db_dir = state_dir "db" in
    let db = Sqlite3.db_open Fpath.(to_string (db_dir / "sqlite.db")) in
    Sqlite3.busy_timeout db 1000;
    Sqlite3.exec db "PRAGMA journal_mode=WAL" |> or_fail "set write-ahead-log mode";
    Sqlite3.exec db "PRAGMA synchronous=NORMAL" |> or_fail "set synchronous=NORMAL";
    db
  )
