(* Based on https://github.com/let-def/grenier/
   By Frédéric Bour. Relicensed to Apache 2.0 with permission. *)

type t = {
  mutable tag: int;
  mutable prev: t;
  mutable next: t;
  counter: int ref;
  mutable on_forget: (unit -> unit) option;
}

let average x y = (x land y) + (x lxor y) / 2

let curr_index t = t.tag

let rec sentinel = { tag = 0; prev = sentinel; next = sentinel; counter = ref 0; on_forget = None }

let is_first t = t.prev == t
let is_last  t = t == t.next

let is_valid t = t.next != sentinel

let prev_index t =
  if is_first t then
    min_int
  else
    t.prev.tag

let next_index t =
  if is_last t then
    max_int
  else
    t.next.tag

let consistent _ = ()
let consistents _ _ = ()

let root () =
  let rec t = { prev = t; next = t; tag = 0; counter = ref 1; on_forget = None } in
  consistent t;
  t

let forget t =
  if is_valid t then begin
    Option.iter (fun f -> f ()) t.on_forget;
    let {prev; next; counter; _} = t in
    if is_first t then
      next.prev <- next
    else if is_last t then
      prev.next <- prev
    else (
      prev.next <- next;
      next.prev <- prev;
    );
    decr counter;
    t.next <- sentinel;
    t.prev <- sentinel;
    consistent prev;
    consistent next;
    assert (not (is_valid t));
  end

let same_order t1 t2 =
  is_valid t1 &&
  is_valid t2 &&
  t1.counter == t2.counter

let compare t1 t2 =
  assert (same_order t1 t2);
  compare t1.tag t2.tag

let cardinal t = if is_valid t then !(t.counter) else 0

let uint_size = Sys.word_size - 2
let pow = 2.0 ** float uint_size
let inv = 1.0 /. float uint_size

let optimal_t count = (pow /. float count) ** inv

let find_span n =
  let t = optimal_t !(n.counter) in
  let count = ref 1
  and left  = ref n
  and right = ref n
  and tag   = n.tag
  and low   = ref n.tag
  and high  = ref n.tag
  and bit = ref 1
  and thresh = ref 1.0
  in
  while !bit > 0 && (float !count >= float !bit *. !thresh) do
    let to_left = (tag land !bit) <> 0 in
    if to_left then begin
      low := !low lxor !bit;
      while !left.tag > !low && not (is_first !left) do
        left := !left.prev;
        incr count;
      done
    end else begin
      high := !high lxor !bit;
      while !right.tag < !high && not (is_last !right) do
        right := !right.next;
        incr count;
      done
    end;
    bit := !bit lsl 1;
    thresh := !thresh /. t;
  done;
  !left, !low, (!bit lsr 1), !count

let rec relabel_span_big root step tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root || is_last root)
  | n ->
    root.tag <- tag;
    assert (tag > prev_index root);
    relabel_span_big root.next step (tag + step) (n - 1)

let rec relabel_span_small node root slack tag = function
  | 1 ->
    root.tag <- tag;
    assert (tag < next_index root || is_last root)
  | n ->
    root.tag <- tag;
    (*Printf.eprintf "assert (%d > %d); slack = %d\n"
      tag (prev_index root) slack;*)
    assert (tag > prev_index root);
    relabel_span_small node root.next slack
      (tag + if node == root then slack + 1 else 1) (n - 1)

let relabel node =
  let root, tag, range, count = find_span node in
  let step = range / count in
  (*Printf.eprintf "range = %d, count = %d\n" range count;*)
  if step <= 1 then
    (assert (range >= count);
     relabel_span_small node root (range - count) (tag + 1) count)
  else
    relabel_span_big root step (tag + step) count;
  consistents root count

let after ?on_forget t =
  assert (is_valid t);
  let tag = average (curr_index t) (next_index t) in
  let t' = {prev = t; next = t; tag; counter = t.counter; on_forget} in
  let {next; counter; _} = t in
  if t == next then
    t'.next <- t'
  else (
    t'.next <- next;
    next.prev <- t'
  );
  t.next <- t';
  incr counter;
  if t'.tag = prev_index t' then
    relabel t';
  consistent t;
  consistent t';
  t'

let splice_out ts te =
  if ts == te then ()
  else (
    assert (compare ts te < 0);
    let rec aux t =
      if t != te then (
        let next = t.next in
        forget t;
        aux next
      )
    in
    aux ts.next
  )

let set_forget t fn =
  assert (is_valid t);
  assert (t.on_forget = None);
  t.on_forget <- Some fn

let clear_forget t =
  assert (is_valid t);
  assert (t.on_forget <> None);
  t.on_forget <- None

let next t =
  assert (not (is_last t));
  t.next

let prev t =
  assert (not (is_first t));
  t.prev
