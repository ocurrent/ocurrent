type 'a t = {
  md : Static.t;
  fn : 'a Dyn.t;
}

let make md fn =
  { md; fn }

let pending =
  make Static.pending Dyn.pending

let return x =
  make (Static.return ()) (Dyn.return x)

let fail msg =
  make (Static.fail ()) (Dyn.fail msg)

let bind ?(name="?") (f:'a -> 'b t) (x:'a t) =
  let md = Static.bind ~name x.md in
  match Dyn.run x.fn with
  | Error (`Msg e) -> make md (Dyn.fail e)
  | Error (`Pending) -> make md Dyn.pending
  | Ok y -> Static.with_context md f y

let map f x =
  let fn = Dyn.map f x.fn in
  make x.md fn

let pair a b =
  let md = Static.pair a.md b.md in
  let fn = Dyn.pair a.fn b.fn in
  make md fn

module Syntax = struct
  let (let**) x f name = bind ~name f x
  let (let*) x f = bind f x
  let (and*) = pair

  let (let+) x f = map f x
  let (and+) = pair
end

open Syntax

let rec all = function
  | [] -> return ()
  | [x] -> x
  | x :: xs ->
    let+ () = x
    and+ () = all xs in
    ()

let list_map f xs =
  match Dyn.run xs.fn with
  | Error _ ->
    (* Not ready; use static version of map. *)
    let f = f pending in
    let md = Static.list_map ~f:f.md xs.md in
    make md Dyn.pending
  | Ok items ->
    (* Ready. Expand inputs. *)
    let rec aux = function
      | [] -> return []
      | x :: xs ->
        let+ y = f (return x)
        and+ ys = aux xs in
        y :: ys
    in
    let results = aux items in
    { results with md = Static.list_map ~f:results.md xs.md }

let list_iter f xs =
  let+ (_ : unit list) = list_map f xs in
  ()

let gate ~on t =
  let md = Static.gate ~on:on.md t.md in
  let fn =
    Dyn.bind on.fn @@ fun () ->
    t.fn
  in
  make md fn

let pp f x = Static.pp f x.md
let pp_dot f x = Static.pp_dot f x.md

let run x =
  Dyn.run x.fn

let analyse x = x.md
