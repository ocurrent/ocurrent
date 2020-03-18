module S = S
module Output = Output

module Make (Input : S.INPUT) = struct
  module An = Analysis.Make(struct type id = Input.job_id end)

  type 'a node = {
    md : An.t;
    fn : 'a Dyn.t;
  }

  type context = unit ref

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
    fun ctx ->
      match !last with
      | Some (prev_ctx, cached) when prev_ctx == ctx -> cached
      | _ ->
        let r = f ctx in
        last := Some (ctx, r);
        r

  let active s =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.active ~env s) (Dyn.active s)

  let return ?label x =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.return ~env label) (Dyn.return x)

  let map_input ~label source x =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.map_input ~env source label) (Dyn.of_output x)

  let option_input ~label source x =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.option_input ~env source label) (Dyn.of_output x)

  let fail msg =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.fail ~env msg) (Dyn.fail msg)

  let state ?(hidden=false) t =
    let env = !bind_context in
    cache @@ fun ctx ->
    let t = t ctx in
    let an = An.state ~env ~hidden t.md in
    make an (Dyn.state t.fn)

  let catch ?(hidden=false) t =
    let env = !bind_context in
    cache @@ fun ctx ->
    let t = t ctx in
    let an = An.catch ~env ~hidden t.md in
    make an (Dyn.catch t.fn)

  let of_output x =
    let env = !bind_context in
    cache @@ fun _ctx ->
    make (An.of_output ~env x) (Dyn.of_output x)

  let component fmt = Fmt.strf ("@[<v>" ^^ fmt ^^ "@]")

  let bind ?info (f:'a -> 'b t) (x:'a t) =
    let env = !bind_context in
    cache @@ fun ctx ->
    let x = x ctx in
    let md = An.bind ~env ?info x.md in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (md (An.Fail e)) (Dyn.fail e)
    | Error (`Active a) -> make (md (An.Active a)) (Dyn.active a)
    | Ok y ->
      let md = md An.Pass in
      let f2 = with_bind_context md f y in
      f2 ctx

  let msg_of_exn = function
    | Failure m -> m
    | ex -> Printexc.to_string ex

  let map f x =
    let env = !bind_context in
    cache @@ fun ctx ->
    let x = x ctx in
    match Dyn.map f x.fn with
    | fn -> make x.md fn
    | exception ex ->
      let msg = msg_of_exn ex in
      make (An.map_failed ~env x.md msg) (Dyn.fail msg)

  let map_error f x =
    let env = !bind_context in
    cache @@ fun ctx ->
    let x = x ctx in
    match Dyn.map_error f x.fn with
    | fn -> make x.md fn
    | exception ex ->
      let msg = msg_of_exn ex in
      make (An.map_failed ~env x.md msg) (Dyn.fail msg)

  let ignore_value x = map ignore x

  let pair a b =
    let env = !bind_context in
    cache @@ fun ctx ->
    let a = a ctx in
    let b = b ctx in
    let md = An.pair ~env a.md b.md in
    let fn = Dyn.pair a.fn b.fn in
    make md fn

  let bind_input ~info (f:'a -> 'b Input.t) (x:'a t) =
    let env = !bind_context in
    cache @@ fun ctx ->
    let x = x ctx in
    match Dyn.run x.fn with
    | Error (`Msg e) -> make (An.bind_input ~env ~info x.md (An.Fail e)) (Dyn.fail e)
    | Error (`Active a) -> make (An.bind_input ~env ~info x.md (An.Active a)) (Dyn.active a)
    | Ok y ->
      let input = f y in
      let v, id = Input.get input in
      let md = An.bind_input ~env ~info x.md ?id (
          match v with
          | Error (`Msg e) -> An.Fail e
          | Error (`Active a) -> An.Active a
          | Ok _ -> An.Pass
        )
      in
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

  let all_labelled items =
    let rec aux = function
      | [] -> return (Ok ())
      | (l, x) :: xs ->
        let+ x = catch x ~hidden:true
        and+ xs = aux xs in
        match x with
        | Ok () -> xs
        | Error (`Msg e) ->
          match xs with
          | Ok () -> Error (`Same ([l], e))
          | Error (`Same (ls, e2)) when e = e2 -> Error (`Same (l :: ls, e))
          | Error (`Same (ls, _))
          | Error (`Diff ls) -> Error (`Diff (l :: ls))
    in
    "all" |>
    let** results = aux items in
    match results with
    | Ok () -> return ()
    | Error (`Same (ls, e)) -> fail (Fmt.strf "%a failed: %s" Fmt.(list ~sep:(unit ", ") string) ls e)
    | Error (`Diff ls) -> fail (Fmt.strf "%a failed" Fmt.(list ~sep:(unit ", ") string) ls)

  let option_map (f : 'a t -> 'b t) (input : 'a option t) =
    let env = !bind_context in
    cache @@ fun ctx ->
    let input = input ctx in
    match Dyn.run input.fn with
    | Error _ as r ->
      (* Not ready; use static version. *)
      let f = f (option_input ~label:`Blocked input.md r) ctx in
      let md = An.option_map ~env ~f:f.md input.md in
      make md (Dyn.of_output r)
    | Ok None ->
      (* Show what would have been done. *)
      let r = Error (`Msg "(none)") in
      let f = f (option_input input.md ~label:`Not_selected r) ctx in
      let md = An.option_map ~env ~f:f.md input.md in
      make md (Dyn.of_output (Ok None))
    | Ok (Some item) ->
      let results =
        (let+ y = f (option_input input.md ~label:`Selected (Ok item)) in Some y) ctx
      in
      { results with md = An.option_map ~env ~f:results.md input.md }

  let list_map ~pp (f : 'a t -> 'b t) (input : 'a list t) =
    let env = !bind_context in
    cache @@ fun ctx ->
    let input = input ctx in
    match Dyn.run input.fn with
    | Error _ as r ->
      (* Not ready; use static version of map. *)
      let f = f (map_input input.md ~label:(Error `Blocked) r) ctx in
      let md = An.list_map ~env ~f:f.md input.md in
      make md (Dyn.of_output r)
    | Ok [] ->
      (* Empty list; show what would have been done. *)
      let no_items = Error (`Msg "(empty list)") in
      let f = f (map_input input.md ~label:(Error `Empty_list) no_items) ctx in
      let md = An.list_map ~env ~f:f.md input.md in
      make md (Dyn.return [])
    | Ok items ->
      (* Ready. Expand inputs. *)
      let rec aux = function
        | [] -> return []
        | x :: xs ->
          let+ y = f (map_input ~label:(Ok (Fmt.to_to_string pp x)) input.md (Ok x))
          and+ ys = aux xs in
          y :: ys
      in
      let results = aux items ctx in
      { results with md = An.list_map ~env ~f:results.md input.md }

  let list_iter ~pp f xs =
    let+ (_ : unit list) = list_map ~pp f xs in
    ()

  let rec list_seq : 'a t list -> 'a list t = function
    | [] -> return []
    | x :: xs ->
      let+ y = x
      and+ ys = list_seq xs in
      y :: ys

  let option_seq : 'a t option -> 'a option t = function
    | None -> return None
    | Some x -> let+ y = x in Some y

  let gate ~on t =
    let env = !bind_context in
    cache @@ fun ctx ->
    let t = t ctx in
    let on = on ctx in
    let md = An.gate ~env ~on:on.md t.md in
    let fn =
      Dyn.bind on.fn @@ fun () ->
      t.fn
    in
    make md fn

  module Executor = struct
    let run f =
      let ctx = ref () in
      try
        let x = f () ctx in
        Dyn.run x.fn, x.md
      with ex ->
        let msg = Printexc.to_string ex in
        let fn = Dyn.fail msg |> Dyn.run in
        let md = An.fail ~env:None msg in
        fn, md
  end

  module Analysis = struct
    include An

    let get t =
      cache @@ fun ctx ->
      let t = t ctx in
      make t.md @@ Dyn.return t.md
  end
end
