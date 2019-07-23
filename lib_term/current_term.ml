module S = S
module Output = Output

module Make (Input : S.INPUT) = struct
  module An = Analysis.Make(struct type id = Input.job_id end)

  type 'a node = {
    md : An.t;
    fn : 'a Dyn.t;
  }

  type context = {
    user_env : Input.env;
    default_env : An.env;
  }

  type 'a t = context -> 'a node

  type description = string

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
          | None -> ctx.default_env
          | Some b -> An.with_bind b ctx.default_env
        in
        let r = f ~env ctx in
        last := Some (ctx, r);
        r

  let blocked () =
    cache @@ fun ~env _ctx ->
    make (An.blocked ~env ()) Dyn.pending

  let pending () =
    cache @@ fun ~env _ctx ->
    make (An.pending ~env ()) Dyn.pending

  let return ?label x =
    cache @@ fun ~env _ctx ->
    make (An.return ~env label) (Dyn.return x)

  let fail msg =
    cache @@ fun ~env _ctx ->
    make (An.fail ~env ()) (Dyn.fail msg)

  let state t =
    cache @@ fun ~env ctx ->
    let t = t ctx in
    make (An.state ~env t.md) (Dyn.state t.fn)

  let catch t =
    cache @@ fun ~env ctx ->
    let t = t ctx in
    make (An.catch ~env t.md) (Dyn.catch t.fn)

  let of_output x =
    cache @@ fun ~env _ctx ->
    make (An.of_output ~env x) (Dyn.of_output x)

  let component = Fmt.strf

  let bind ?info (f:'a -> 'b t) (x:'a t) =
    cache @@ fun ~env ctx ->
    let x = x ctx in
    let md = An.bind ~env ?info x.md in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (md An.Fail) (Dyn.fail e)
    | Error (`Pending) -> make (md An.Active) Dyn.pending
    | Ok y ->
      let md = md An.Pass in
      let f2 = with_bind_context md f y in
      let r = f2 ctx in
      An.set_state md (
        match Dyn.run r.fn with
        | Error (`Msg _) -> An.Fail
        | Error (`Pending) -> An.Active
        | Ok _ -> An.Pass
      );
      r

  let map f x =
    cache @@ fun ~env:_ ctx ->
    let x = x ctx in
    let fn = Dyn.map f x.fn in
    make x.md fn

  let ignore_value x = map ignore x

  let pair a b =
    cache @@ fun ~env ctx ->
    let a = a ctx in
    let b = b ctx in
    let md = An.pair ~env a.md b.md in
    let fn = Dyn.pair a.fn b.fn in
    make md fn

  let bind_input ~info (f:'a -> 'b Input.t) (x:'a t) =
    cache @@ fun ~env ctx ->
    let x = x ctx in
    let md = An.bind_input ~env ~info x.md in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (md An.Fail) (Dyn.fail e)
    | Error (`Pending) -> make (md An.Active) Dyn.pending
    | Ok y ->
      let md = md An.Pass in
      let input = f y in
      let v, id = Input.get ctx.user_env input in
      An.set_state md ?id (
        match v with
        | Error (`Msg _) -> An.Fail
        | Error (`Pending) -> An.Active
        | Ok _ -> An.Pass
      );
      make md (Dyn.of_output v)

  module Syntax = struct
    let (let**) x f info = bind ~info f x

    let (let>) x f info = bind_input ~info f x
    let (and>) = pair

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

  let list_map ~pp f xs =
    cache @@ fun ~env ctx ->
    let xs = xs ctx in
    match Dyn.run xs.fn with
    | Error _ ->
      (* Not ready; use static version of map. *)
      let f = f (blocked ()) ctx in
      let md = An.list_map ~env ~f:f.md xs.md in
      make md Dyn.pending
    | Ok items ->
      (* Ready. Expand inputs. *)
      let rec aux = function
        | [] -> return []
        | x :: xs ->
          let+ y = f (return ~label:(Fmt.to_to_string pp x) x)
          and+ ys = aux xs in
          y :: ys
      in
      let results = aux items ctx in
      { results with md = An.list_map ~env ~f:results.md xs.md }

  let list_iter ~pp f xs =
    let+ (_ : unit list) = list_map ~pp f xs in
    ()

  let option_seq : 'a t option -> 'a option t = function
    | None -> return None
    | Some x -> let+ y = x in Some y

  let gate ~on t =
    cache @@ fun ~env ctx ->
    let t = t ctx in
    let on = on ctx in
    let md = An.gate ~env ~on:on.md t.md in
    let fn =
      Dyn.bind on.fn @@ fun () ->
      t.fn
    in
    make md fn

  let env =
    cache @@ fun ~env ctx ->
    make (An.return ~env None) (Dyn.return ctx.user_env)

  module Executor = struct
    let run ~env:user_env f =
      let default_env = An.make_env () in
      let ctx = { default_env; user_env } in
      try
        let x = f () ctx in
        Dyn.run x.fn, x.md
      with ex ->
        let fn = Dyn.fail (Printexc.to_string ex) |> Dyn.run in
        let md = An.fail ~env:default_env () in
        fn, md
  end

  module Analysis = struct
    include An

    let get t =
      cache @@ fun ~env:_ ctx ->
      let t = t ctx in
      make t.md @@ Dyn.return t.md
  end
end
