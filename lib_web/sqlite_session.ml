module Db = Current.Db

type key = string
type value = string
type period = int64

let default_period = Int64.of_int (60 * 60 * 24 * 7)

type t = {
  get : Sqlite3.stmt;
  set : Sqlite3.stmt;
  clear : Sqlite3.stmt;
  expire : Sqlite3.stmt;
  mutable next_expire_due : Int64.t;
}

let gensym () =
  Base64.encode_exn (Cstruct.to_string (Mirage_crypto_rng.generate 30))

let or_fail label x =
  match x with
  | Sqlite3.Rc.OK -> ()
  | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

let now () =
  Int64.of_float (Unix.time ())

let clear t key =
  Db.exec t.clear Sqlite3.Data.[ TEXT key ]

let expire_old t =
  Db.exec t.expire Sqlite3.Data.[ INT (now ()) ]

let get t key =
  match Db.query_some t.get Sqlite3.Data.[ TEXT key ] with
  | None -> Error Session.S.Not_found
  | Some Sqlite3.Data.[ value; INT expires ] ->
    let period = Int64.(sub expires (now ())) in
    if Int64.compare period 0L < 0 then (
      clear t key;
      Error Session.S.Not_found
    ) else (
      match value with
      | NULL       -> Error Session.S.Not_set
      | TEXT value -> Ok (value, period)
      | _ -> Fmt.failwith "Invalid value in row!"
    )
  | Some row -> Fmt.failwith "get: invalid row: %a" Db.dump_row row

let _set ?expiry ?value t key =
  let expiry =
    match expiry with
    | None        -> Int64.(add (now ()) default_period)
    | Some expiry -> Int64.(add (now ()) expiry)
  in
  let value =
    match value with
    | None -> Sqlite3.Data.NULL
    | Some value -> Sqlite3.Data.TEXT value
  in
  Db.exec t.set Sqlite3.Data.[ TEXT key; value; INT expiry ]

let set ?expiry t key value =
  _set ?expiry ~value t key

let generate ?expiry ?value t =
  let now = now () in
  if t.next_expire_due <= now then (
    expire_old t;
    t.next_expire_due <- now;
  );
  let key = gensym () in
  _set ?expiry ?value t key;
  key

let create db =
  Sqlite3.exec db "CREATE TABLE IF NOT EXISTS web_sessions ( \
                   key       TEXT NOT NULL, \
                   value     TEXT, \
                   expires   INTEGER NOT NULL, \
                   PRIMARY KEY (key))" |> or_fail "create session table";
  let get = Sqlite3.prepare db "SELECT value, expires FROM web_sessions WHERE key = ?" in
  let set = Sqlite3.prepare db "INSERT OR REPLACE INTO web_sessions \
                                (key, value, expires) \
                                VALUES (?, ?, ?)" in
  let expire = Sqlite3.prepare db "DELETE FROM web_sessions WHERE expires < ?" in
  let clear = Sqlite3.prepare db "DELETE FROM web_sessions WHERE key = ?" in
  let next_expire_due = Int64.add default_period (now ()) in
  let t = { get; set; clear; expire; next_expire_due } in
  expire_old t;
  t

let default_period _ = default_period
