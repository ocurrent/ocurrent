module S = S
module Output = Output

module Make (Metadata : sig type t end) = struct
  type description = string

  type 'a primitive = ('a Output.t * Metadata.t option) Current_incr.t

  module Node = Node.Make(Metadata)
  open Node

  type 'a t = 'a Node.t

  module Quick_stats = struct
    let v =
      ref { S.
            ok = 0;
            waiting_for_confirmation = 0;
            ready = 0;
            running = 0;
            failed = 0;
            blocked = 0;  (* Calculated from [quick_stats_total] *)
          }

    (* The expected total of all the values in [quick_stats].
       If [v] doesn't add up to this, the missing ones are assumed to be blocked. *)
    let total = ref 0

    let dec_ok ()      = v := { !v with ok = !v.ok - 1 }
    let dec_waiting_for_confirmation () = v := { !v with waiting_for_confirmation = !v.waiting_for_confirmation - 1 }
    let dec_ready ()   = v := { !v with ready = !v.ready - 1 }
    let dec_running () = v := { !v with running = !v.running - 1 }
    let dec_failed ()  = v := { !v with failed = !v.failed - 1 }

    let update ~id : _ Dyn.t -> unit = function
      | Ok _                        -> v := { !v with ok = !v.ok + 1 }; Current_incr.on_release dec_ok
      | Error (src, _) when not (Id.equal src id) -> ()
      | Error (_, `Active `Waiting_for_confirmation) -> v := { !v with waiting_for_confirmation = !v.waiting_for_confirmation + 1 }; Current_incr.on_release dec_waiting_for_confirmation
      | Error (_, `Active `Ready)   -> v := { !v with ready = !v.ready + 1 }; Current_incr.on_release dec_ready
      | Error (_, `Active `Running) -> v := { !v with running = !v.running + 1 }; Current_incr.on_release dec_running
      | Error (_, `Msg _)           -> v := { !v with failed = !v.failed + 1 }; Current_incr.on_release dec_failed

    let dec_total () = decr total

    let update_total () =
      incr total;
      Current_incr.on_release dec_total

    let get () =
      let v = !v in
      { v with blocked = !total - v.ok - v.ready - v.running - v.failed }
  end

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

  let incr_map ?eq fn v =
    let open Current_incr in
    of_cc begin
      read v @@ fun x ->
      write ~eq:(Dyn.equal ?eq) (fn x)
    end

  let state ?(hidden=false) t =
    let eq = Output.equal (==) in
    node (State { source = Term t; hidden }) @@ incr_map ~eq Dyn.state t.v

  let catch ?(hidden=false) t =
    let eq = Result.equal ~ok:(==) ~error:(==) in
    node (Catch { source = Term t; hidden }) @@ incr_map ~eq Dyn.catch t.v

  let component fmt = Fmt.str ("@[<v>" ^^ fmt ^^ "@]")

  let join ?eq x =
    Current_incr.of_cc begin
      Current_incr.read x @@ fun y ->
      Current_incr.read y.v @@ Current_incr.write ~eq:(Dyn.equal ?eq)
    end

  let bind ?(info="") (f:'a -> 'b t) (x:'a t) =
    Quick_stats.update_total ();
    let bind_in = node (Bind_in (Term x, info)) x.v in
    let t =
      x.v |> Current_incr.map @@ fun v ->
      Quick_stats.update ~id:x.id v;
      with_bind_context (Term bind_in) @@ fun () ->
      match v with
      | Error _ as e -> node (Constant None) @@ Current_incr.const e
      | Ok y -> f y
    in
    let nested = Current_incr.map (fun t -> Term t) t in
    node (Bind_out nested) (join t)

  let map f x =
    let id = Id.mint () in
    node ~id (Map (Term x)) @@ incr_map (Dyn.map ~id f) x.v

  let map_error f x =
    let id = Id.mint () in
    node ~id (Map (Term x)) @@ incr_map (Dyn.map_error ~id f) x.v

  let ignore_value x = map ignore x

  let pair a b =
    node (Pair (Term a, Term b)) @@ Current_incr.of_cc begin
      Current_incr.read a.v @@ fun a ->
      Current_incr.read b.v @@ fun b ->
      Current_incr.write @@ Dyn.pair a b
    end

  let primitive ~info (f:'a -> 'b primitive) (x:'a t) =
    Quick_stats.update_total ();
    let id = Id.mint () in
    let v_meta =
      Current_incr.of_cc begin
        Current_incr.read x.v @@ function
        | Error _ as e -> Current_incr.write (e, None)
        | Ok y ->
          let output = f y in
          Current_incr.read output @@ fun (v, job) ->
          let v = with_id id v in
          Quick_stats.update ~id v;
          Current_incr.write (v, job)
      end
    in
    let v = incr_map fst v_meta in
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

  let with_context (ctx : _ t) f =
    let ctx =
      match !bind_context with
      | None -> Term ctx
      | Some (Term prev) -> Term (pair prev ctx)
    in
    with_bind_context ctx f

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
    | Error (`Same (ls, e)) -> fail (Fmt.str "%a failed: %s" Fmt.(list ~sep:(any ", ") string) ls e)
    | Error (`Diff ls) -> fail (Fmt.str "%a failed" Fmt.(list ~sep:(any ", ") string) ls)

  (* A node with the constant value [v], but that depends on [old]. *)
  let replace old v =
    {
      id = Id.mint ();
      v = Current_incr.const v;
      ty = Constant None;
      bind = Some (Term old)
    }

  let option_map (type a b) ?label (f : a t -> b t) (input : a option t) : b option t =
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
    node (Option_map { item = Term input; output; label }) (join results)

  let rec list_seq : 'a t list -> 'a list t = function
    | [] -> return []
    | x :: xs ->
      let+ y = x
      and+ ys = list_seq xs in
      y :: ys

  let collapse_list ~key ~value ~input t =
    let all_of_them = list_seq t in
    let collapse_node = node (Collapse { key; value; input = Term input; output = Term all_of_them }) all_of_them.v in
    List.map (fun t -> node (Map (Term collapse_node)) t.v) t, collapse_node |> map (fun _ -> ())

  let list_map (type a) (module M : S.ORDERED with type t = a) ?collapse_key ?label (f : a t -> 'b t) (input : a list t) =
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
    node (List_map { items = Term input; output; label }) (join results)

  let list_iter (type a) (module M : S.ORDERED with type t = a) ?collapse_key ?label f (xs : a list t) =
    let+ (_ : unit list) = list_map (module M) ?collapse_key ?label f xs in
    ()

  let option_seq : 'a t option -> 'a option t = function
    | None -> return None
    | Some x -> let+ y = x in Some y

  let gate ~on t =
    let eq = Dyn.equal ~eq:(==) in
    node (Gate_on { ctrl = Term on; value = Term t }) @@ Current_incr.of_cc begin
      Current_incr.read t.v @@ fun t ->
      Current_incr.read on.v @@ fun on ->
      Current_incr.write ~eq @@ Dyn.bind on (fun () -> t)
    end

  let of_output x =
    let id = Id.mint () in
    let x = with_id id x in
    Quick_stats.update_total ();
    Quick_stats.update ~id x;
    node ~id (Constant None) @@ Current_incr.const x

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

    let quick_stat = Quick_stats.get
  end
end
