module S = S

module Make (Input : S.INPUT) = struct
  type 'a node = {
    md : Analysis.t;
    fn : 'a Dyn.t;
  }

  type context = {
    env : Analysis.env;
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
        let env =
          match bind with
          | None -> ctx.env
          | Some b -> Analysis.with_bind b ctx.env
        in
        let r = f ~env ctx in
        last := Some (ctx, r);
        r

  let pending () =
    cache @@ fun ~env _ctx ->
    make (Analysis.pending ~env ()) Dyn.pending

  let return x =
    cache @@ fun ~env _ctx ->
    make (Analysis.return ~env ()) (Dyn.return x)

  let fail msg =
    cache @@ fun ~env _ctx ->
    make (Analysis.fail ~env ()) (Dyn.fail msg)

  let bind ?(name="?") (f:'a -> 'b t) (x:'a t) =
    cache @@ fun ~env ctx ->
    let x = x ctx in
    let md = Analysis.bind ~env ~name x.md in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (md Analysis.Fail) (Dyn.fail e)
    | Error (`Pending) -> make (md Analysis.Active) Dyn.pending
    | Ok y ->
      let md = md Analysis.Pass in
      let f2 = with_bind_context md f y in
      let r = f2 ctx in
      Analysis.set_state md (
        match Dyn.run r.fn with
        | Error (`Msg _) -> Analysis.Fail
        | Error (`Pending) -> Analysis.Active
        | Ok _ -> Analysis.Pass
      );
      r

  let map f x =
    cache @@ fun ~env:_ ctx ->
    let x = x ctx in
    let fn = Dyn.map f x.fn in
    make x.md fn

  let pair a b =
    cache @@ fun ~env ctx ->
    let a = a ctx in
    let b = b ctx in
    let md = Analysis.pair ~env a.md b.md in
    let fn = Dyn.pair a.fn b.fn in
    make md fn

  let track i t =
    cache @@ fun ~env:_ ctx ->
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
    cache @@ fun ~env ctx ->
    let xs = xs ctx in
    match Dyn.run xs.fn with
    | Error _ ->
      (* Not ready; use static version of map. *)
      let f = f (pending ()) ctx in
      let md = Analysis.list_map ~env ~f:f.md xs.md in
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
      { results with md = Analysis.list_map ~env ~f:results.md xs.md }

  let list_iter f xs =
    let+ (_ : unit list) = list_map f xs in
    ()

  let gate ~on t =
    cache @@ fun ~env ctx ->
    let t = t ctx in
    let on = on ctx in
    let md = Analysis.gate ~env ~on:on.md t.md in
    let fn =
      Dyn.bind on.fn @@ fun () ->
      t.fn
    in
    make md fn

  module Executor = struct
    let run x =
      let env = Analysis.make_env () in
      let ctx = { env; inputs = [] } in
      let x = x ctx in
      x.md, Dyn.run x.fn, ctx.inputs

    type 'a output = 'a Dyn.or_error

    let pp_output = Dyn.pp
  end

  module Analysis = Analysis
end
