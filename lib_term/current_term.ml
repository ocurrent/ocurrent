module type INPUT = sig
  type t
end

module Make (Input : INPUT) = struct
  type 'a node = {
    md : Static.t;
    fn : 'a Dyn.t;
  }

  type context = {
    mutable inputs : Input.t list;
  }

  type 'a t = context -> 'a node

  let make md fn =
    { md; fn }

  let bind_context = ref None

  let with_bind_context bc f x =
    assert (!bind_context = None);
    bind_context := Some bc;
    let r = f x in
    bind_context := None;
    r

  let cache f =
    let last = ref None in
    let bind = !bind_context in
    fun ctx ->
      match !last with
      | Some (prev_ctx, cached) when prev_ctx == ctx -> cached
      | _ ->
        let r = f ~bind ctx in
        last := Some (ctx, r);
        r

  let pending () =
    cache @@ fun ~bind _ctx ->
    make (Static.pending ~bind ()) Dyn.pending

  let return x =
    cache @@ fun ~bind _ctx ->
    make (Static.return ~bind ()) (Dyn.return x)

  let fail msg =
    cache @@ fun ~bind _ctx ->
    make (Static.fail ~bind ()) (Dyn.fail msg)

  let bind ?(name="?") (f:'a -> 'b t) (x:'a t) =
    cache @@ fun ~bind ctx ->
    let x = x ctx in
    let md = Static.bind ~bind ~name x.md in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (md Static.Fail) (Dyn.fail e)
    | Error (`Pending) -> make (md Static.Active) Dyn.pending
    | Ok y ->
      let md = md Static.Pass in
      let f2 = with_bind_context md f y in
      let r = f2 ctx in
      Static.set_state md (
        match Dyn.run r.fn with
        | Error (`Msg _) -> Static.Fail
        | Error (`Pending) -> Static.Active
        | Ok _ -> Static.Pass
      );
      r

  let map f x =
    cache @@ fun ~bind:_ ctx ->
    let x = x ctx in
    let fn = Dyn.map f x.fn in
    make x.md fn

  let pair a b =
    cache @@ fun ~bind ctx ->
    let a = a ctx in
    let b = b ctx in
    let md = Static.pair ~bind a.md b.md in
    let fn = Dyn.pair a.fn b.fn in
    make md fn

  let track i t =
    cache @@ fun ~bind:_ ctx ->
    ctx.inputs <- i :: ctx.inputs;
    t ctx

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
    cache @@ fun ~bind ctx ->
    let xs = xs ctx in
    match Dyn.run xs.fn with
    | Error _ ->
      (* Not ready; use static version of map. *)
      let f = f (pending ()) ctx in
      let md = Static.list_map ~bind ~f:f.md xs.md in
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
      let results = aux items ctx in
      { results with md = Static.list_map ~bind ~f:results.md xs.md }

  let list_iter f xs =
    let+ (_ : unit list) = list_map f xs in
    ()

  let gate ~on t =
    cache @@ fun ~bind ctx ->
    let t = t ctx in
    let on = on ctx in
    let md = Static.gate ~bind ~on:on.md t.md in
    let fn =
      Dyn.bind on.fn @@ fun () ->
      t.fn
    in
    make md fn

  let run x =
    let ctx = { inputs = [] } in
    let x = x ctx in
    x.md, Dyn.run x.fn, ctx.inputs

  type 'a output = 'a Dyn.or_error

  let pp_output = Dyn.pp

  module Static = Static
end
