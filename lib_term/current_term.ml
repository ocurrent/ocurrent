module S = S
module Output = Output

module Make (Metadata : sig type t end) = struct
  type description = string

  type 'a primitive = ('a Output.t * Metadata.t option) Current_incr.t

  module Node = Node.Make(Metadata)
  open Node

  type 'a t = 'a Node.t

  let bind_context : bind_context ref = ref None

  let node ?(id=Id.mint ()) ty v = { id; v; ty; bind = !bind_context }

  let with_bind_context bc f =
    let old = !bind_context in
    bind_context := Some bc;
    Fun.protect
      (fun () -> f ())
      ~finally:(fun () -> bind_context := old)

  let with_id id = function
    | Ok _ as v -> v
    | Error e -> Error (id, e)

  let active s =
    let id = Id.mint () in
    node ~id (Constant None) @@ Current_incr.const (Dyn.active ~id s)

  let return ?label x =
    node (Constant label) @@ Current_incr.const (Dyn.return x)

  let map_input ~label source x =
    node (Map_input {source = Term source; info = label}) @@ Current_incr.const x

  let option_input source x =
    node (Opt_input {source = Term source }) @@ Current_incr.const x

  let fail msg =
    let id = Id.mint () in
    node ~id (Constant None) @@ Current_incr.const (Dyn.fail ~id msg)

  let state ?(hidden=false) t =
    node (State { source = Term t; hidden }) @@ Current_incr.map Dyn.state t.v

  let catch ?(hidden=false) t =
    node (Catch { source = Term t; hidden }) @@ Current_incr.map Dyn.catch t.v

  let component fmt = Fmt.strf ("@[<v>" ^^ fmt ^^ "@]")

  let join x =
    Current_incr.of_cc begin
      Current_incr.read x @@ fun y ->
      Current_incr.read y.v Current_incr.write
    end

  let bind ?(info="") (f:'a -> 'b t) (x:'a t) =
    let bind_in = node (Bind_in (Term x, info)) x.v in
    let t =
      x.v |> Current_incr.map @@ fun v ->
      with_bind_context (Term bind_in) @@ fun () ->
      match v with
      | Error _ as e -> node (Constant None) @@ Current_incr.const e
      | Ok y -> f y
    in
    let nested = Current_incr.map (fun t -> Term t) t in
    node (Bind_out nested) (join t)

  let map f x =
    let id = Id.mint () in
    node ~id (Map (Term x)) @@ Current_incr.map (Dyn.map ~id f) x.v

  let map_error f x =
    let id = Id.mint () in
    node ~id (Map (Term x)) @@ Current_incr.map (Dyn.map_error ~id f) x.v

  let ignore_value x = map ignore x

  let pair a b =
    node (Pair (Term a, Term b)) @@ Current_incr.of_cc begin
      Current_incr.read a.v @@ fun a ->
      Current_incr.read b.v @@ fun b ->
      Current_incr.write @@ Dyn.pair a b
    end

  let primitive ~info (f:'a -> 'b primitive) (x:'a t) =
    let id = Id.mint () in
    let v_meta =
      Current_incr.of_cc begin
        Current_incr.read x.v @@ function
        | Error _ as e -> Current_incr.write (e, None)
        | Ok y ->
          let output = f y in
          Current_incr.read output @@ fun (v, job) ->
          Current_incr.write (with_id id v, job)
      end
    in
    let v = Current_incr.map fst v_meta in
    let meta = Current_incr.map snd v_meta in
    node ~id (Primitive { x = Term x; info; meta }) v

  module Syntax = struct
    let (let**) x f info = bind ~info f x

    let (let>) x f info = primitive ~info f x
    let (and>) = pair

    let (let*) x f = bind f x
    let (and*) = pair

    let (let+) x f = map f x
    let (and+) = pair
  end

  open Syntax

  let collapse ~key ~value ~input t =
    node (Collapse { key; value; input = Term input; output = Term t }) t.v

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

  (* A node with the constant value [v], but that depends on [old]. *)
  let replace old v =
    {
      id = Id.mint ();
      v = Current_incr.const v;
      ty = Constant None;
      bind = Some (Term old)
    }

  let option_map (type a b) (f : a t -> b t) (input : a option t) : b option t =
    let results =
      input.v |> Current_incr.map @@ function
      | Error _ as r ->
        (* Not ready; use static version. *)
        let output = f (option_input input r) in
        replace output r
      | Ok None ->
        (* Show what would have been done. *)
        let no_item = Error (Id.mint (), `Active `Ready) in
        let output = f (option_input input no_item) in
        replace output (Ok None)
      | Ok (Some item) ->
        let output = f (option_input input (Ok item)) in
        { output with v = Current_incr.map (Result.map Option.some) output.v }
    in
    let output = Current_incr.map (fun x -> Term x) results in
    node (Option_map { item = Term input; output }) (join results)

  let rec list_seq : 'a t list -> 'a list t = function
    | [] -> return []
    | x :: xs ->
      let+ y = x
      and+ ys = list_seq xs in
      y :: ys


  let list_map (type a) (module M : S.ORDERED with type t = a) ?collapse_key (f : a t -> 'b t) (input : a list t) =
    let module Map = Map.Make(M) in
    let module Sep = Current_incr.Separate(Map) in
    (* Stage 1 : convert input list to a set.
       This runs whenever the input list changes. *)
    let as_map =
      input.v |> Current_incr.map @@ function
      | Ok items -> items |> List.fold_left (fun acc x -> Map.add x () acc) Map.empty
      | _ -> Map.empty
    in
    (* Stage 2 : process each element separately.
       We only process an element when it is first added to the set,
       not on every change to the set. *)
    let results =
      Sep.map as_map @@ fun item ->
      let label = Fmt.to_to_string M.pp item in
      let input = map_input ~label:(Ok label) input (Ok item) in
      let output = f input in
      match collapse_key with
      | None -> Current_incr.write output
      | Some key -> Current_incr.write (collapse ~key ~value:label ~input output)
    in
    (* Stage 3 : combine results.
       This runs whenever either the set of results changes, or the input list changes
       (since the output order might need to change). *)
    let results =
      Current_incr.of_cc begin
        Current_incr.read input.v @@ function
        | Error _ as r ->
          (* Not ready; use static version of map. *)
          let output = f (map_input input ~label:(Error `Blocked) r) in
          Current_incr.write @@ replace output r
        | Ok [] ->
          (* Empty list; show what would have been done. *)
          let no_items = Error (Id.mint (), `Active `Ready) in
          let output = f (map_input input ~label:(Error `Empty_list) no_items) in
          Current_incr.write @@ replace output (Ok [])
        | Ok items ->
          Current_incr.read results @@ fun results ->
          (* Convert result set to a results list. *)
          let results = items |> List.map (fun item -> Map.find item results) |> list_seq in
          Current_incr.write results
      end
    in
    let output = Current_incr.map (fun x -> Term x) results in
    node (List_map { items = Term input; output }) (join results)

  let list_iter (type a) (module M : S.ORDERED with type t = a) ?collapse_key f (xs : a list t) =
    let+ (_ : unit list) = list_map (module M) ?collapse_key f xs in
    ()

  let option_seq : 'a t option -> 'a option t = function
    | None -> return None
    | Some x -> let+ y = x in Some y

  let gate ~on t =
    node (Gate_on { ctrl = Term on; value = Term t }) @@ Current_incr.of_cc begin
      Current_incr.read t.v @@ fun t ->
      Current_incr.read on.v @@ fun on ->
      Current_incr.write @@ Dyn.bind on (fun () -> t)
    end

  let of_output x =
    let id = Id.mint () in
    node ~id (Constant None) @@ Current_incr.const (with_id id x)

  module Executor = struct
    let run (t : 'a t) = Current_incr.map Dyn.run t.v
  end

  module Analysis = struct
    include Analysis.Make(Metadata)

    (* This is a bit of a hack. *)
    let metadata t =
      let rec aux (Term t) =
        match t.ty with
        | Primitive p -> p.meta
        | Map t -> aux t
        | _ -> failwith "metadata: this is not a primitive term!"
      in
      node (Constant None) @@ Current_incr.map Result.ok @@ aux (Term t)
  end
end
