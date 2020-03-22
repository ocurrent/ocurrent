module S = S
module Output = Output

module Make (Input : S.INPUT) = struct
  module An = Analysis.Make(struct type id = Input.job_id end)

  type 'a node = {
    md : An.t;
    fn : 'a Dyn.t;
  }

  type 'a t = 'a node Current_incr.t

  type description = string

  let make md fn =
    Current_incr.const { md; fn }

  let make_cc md fn =
    Current_incr.write { md; fn }

  let bind_context = ref None

  let with_bind_context bc f x =
    let old = !bind_context in
    bind_context := Some bc;
    Fun.protect
      (fun () -> f x)
      ~finally:(fun () -> bind_context := old)

  let active s =
    let env = !bind_context in
    make (An.active ~env s) (Dyn.active s)

  let return ?label x =
    let env = !bind_context in
    make (An.return ~env label) (Dyn.return x)

  let map_input ~label source x =
    let env = !bind_context in
    make (An.map_input ~env source label) (Dyn.of_output x)

  let option_input ~label source x =
    let env = !bind_context in
    make (An.option_input ~env source label) (Dyn.of_output x)

  let fail msg =
    let env = !bind_context in
    make (An.fail ~env msg) (Dyn.fail msg)

  let state ?(hidden=false) t =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read t @@ fun t ->
      let an = An.state ~env ~hidden t.md in
      make_cc an (Dyn.state t.fn)
    end

  let catch ?(hidden=false) t =
    Current_incr.of_cc begin
      let env = !bind_context in
      Current_incr.read t @@ fun t ->
      let an = An.catch ~env ~hidden t.md in
      make_cc an (Dyn.catch t.fn)
    end

  let of_output x =
    let env = !bind_context in
    make (An.of_output ~env x) (Dyn.of_output x)

  let component fmt = Fmt.strf ("@[<v>" ^^ fmt ^^ "@]")

  let bind ?info (f:'a -> 'b t) (x:'a t) =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read x @@ fun x ->
      let md = An.bind ~env ?info x.md in
      match Dyn.run x.fn with
      | Error (`Msg e) -> make_cc (md (An.Fail e)) (Dyn.fail e)
      | Error (`Active a) -> make_cc (md (An.Active a)) (Dyn.active a)
      | Ok y ->
        let md = md An.Pass in
        let f2 = with_bind_context md f y in
        Current_incr.read f2 @@ fun r ->
        Current_incr.write r
    end

  let msg_of_exn = function
    | Failure m -> m
    | ex -> Printexc.to_string ex

  let map f x =
    Current_incr.of_cc begin
      let env = !bind_context in
      Current_incr.read x @@ fun x ->
      match Dyn.map f x.fn with
      | fn -> make_cc x.md fn
      | exception ex ->
        let msg = msg_of_exn ex in
        make_cc (An.map_failed ~env x.md msg) (Dyn.fail msg)
    end

  let map_error f x =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read x @@ fun x ->
      match Dyn.map_error f x.fn with
      | fn -> make_cc x.md fn
      | exception ex ->
        let msg = msg_of_exn ex in
        make_cc (An.map_failed ~env x.md msg) (Dyn.fail msg)
    end

  let ignore_value x = map ignore x

  let pair a b =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read a @@ fun a ->
      Current_incr.read b @@ fun b ->
      let md = An.pair ~env a.md b.md in
      let fn = Dyn.pair a.fn b.fn in
      make_cc md fn
    end

  let bind_input ~info (f:'a -> 'b Input.t) (x:'a t) =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read x @@ fun x ->
      let md = An.bind_input ~env ~info x.md in
      match Dyn.run x.fn with
      | Error (`Msg e) -> make_cc (md (An.Fail e)) (Dyn.fail e)
      | Error (`Active a) -> make_cc (md (An.Active a)) (Dyn.active a)
      | Ok y ->
        let input = f y in
        Current_incr.read (Input.get input) @@ fun (v, id) ->
        let md = An.bind_input ~env ~info x.md ?id (
            match v with
            | Error (`Msg e) -> An.Fail e
            | Error (`Active a) -> An.Active a
            | Ok _ -> An.Pass
          )
        in
        make_cc md (Dyn.of_output v)
    end

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

  let option_map (f : 'a t -> 'b t) (input : 'a option t) : 'b option t =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read input @@ fun input ->
      match Dyn.run input.fn with
      | Error _ as r ->
        (* Not ready; use static version. *)
        Current_incr.read (f (option_input ~label:`Blocked input.md r)) @@ fun f ->
        let md = An.option_map ~env ~f:f.md input.md in
        make_cc md (Dyn.of_output r)
      | Ok None ->
        (* Show what would have been done. *)
        let r = Error (`Msg "(none)") in
        Current_incr.read (f (option_input input.md ~label:`Not_selected r)) @@ fun f ->
        let md = An.option_map ~env ~f:f.md input.md in
        make_cc md (Dyn.of_output (Ok None))
      | Ok (Some item) ->
        Current_incr.read (f (option_input input.md ~label:`Selected (Ok item))) @@ fun (results : 'b node) ->
        let fn = Dyn.map Option.some results.fn in
        Current_incr.write { fn; md = An.option_map ~env ~f:results.md input.md }
    end

  let list_map ~pp (f : 'a t -> 'b t) (input : 'a list t) =
    let env = !bind_context in
    Current_incr.of_cc begin
      Current_incr.read input @@ fun input ->
      match Dyn.run input.fn with
      | Error _ as r ->
        (* Not ready; use static version of map. *)
        Current_incr.read (f (map_input input.md ~label:(Error `Blocked) r)) @@ fun f ->
        let md = An.list_map ~env ~f:f.md input.md in
        make_cc md (Dyn.of_output r)
      | Ok [] ->
        (* Empty list; show what would have been done. *)
        let no_items = Error (`Msg "(empty list)") in
        Current_incr.read (f (map_input input.md ~label:(Error `Empty_list) no_items)) @@ fun f ->
        let md = An.list_map ~env ~f:f.md input.md in
        make_cc md (Dyn.return [])
      | Ok items ->
        (* Ready. Expand inputs. *)
        let rec aux = function
          | [] -> return []
          | x :: xs ->
            let+ y = f (map_input ~label:(Ok (Fmt.to_to_string pp x)) input.md (Ok x))
            and+ ys = aux xs in
            y :: ys
        in
        Current_incr.read (aux items) @@ fun results ->
        Current_incr.write { results with md = An.list_map ~env ~f:results.md input.md }
    end

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
    Current_incr.of_cc begin
      Current_incr.read t @@ fun t ->
      Current_incr.read on @@ fun on ->
      let md = An.gate ~env ~on:on.md t.md in
      let fn =
        Dyn.bind on.fn @@ fun () ->
        t.fn
      in
      make_cc md fn
    end

  module Executor = struct
    let run (f : unit -> 'a t) =
      try
        Current_incr.of_cc begin
          Current_incr.read (f ()) @@ fun { md; fn } ->
          Current_incr.write (Dyn.run fn, md)
        end
      with ex ->
        let msg = Printexc.to_string ex in
        let fn = Dyn.fail msg in
        let md = An.fail ~env:None msg in
        Current_incr.const (Dyn.run fn, md)
  end

  module Analysis = struct
    include An

    let get t =
      Current_incr.of_cc begin
        Current_incr.read t @@ fun t ->
        make_cc t.md @@ Dyn.return t.md
      end
  end
end
