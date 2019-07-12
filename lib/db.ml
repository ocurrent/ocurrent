type t = Sqlite3.db

let or_fail ~cmd x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3: %s (executing %S)" (Sqlite3.Rc.to_string err) cmd

let no_callback _ = failwith "[exec] used with a query!"

let exec_stmt ?(cb=no_callback) stmt =
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

let exec_literal db sql =
  Sqlite3.exec db sql |> or_fail ~cmd:sql

let bind stmt values =
  Sqlite3.reset stmt |> or_fail ~cmd:"reset";
  List.iteri (fun i v -> Sqlite3.bind stmt (i + 1) v |> or_fail ~cmd:"bind") values

let exec stmt values =
  bind stmt values;
  exec_stmt stmt

let query stmt values =
  bind stmt values;
  let results = ref [] in
  let cb row =
    results := row :: !results
  in
  exec_stmt ~cb stmt;
  List.rev !results

let query_one stmt values =
  match query stmt values with
  | [row] -> row
  | [] -> failwith "No results from SQL query!"
  | _ -> failwith "Multiple results from SQL query!"

let query_some stmt values =
  match query stmt values with
  | [] -> None
  | [row] -> Some row
  | _ -> failwith "Multiple results from SQL query!"

let v =
  lazy (
    let db_dir = Disk_store.state_dir "db" in
    let db = Sqlite3.db_open Fpath.(to_string (db_dir / "sqlite.db")) in
    Sqlite3.busy_timeout db 1000;
    exec_literal db "PRAGMA journal_mode=WAL";
    exec_literal db "PRAGMA synchronous=NORMAL";
    db
  )
