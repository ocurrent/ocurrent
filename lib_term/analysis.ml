module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

module Id : sig
  type t
  val mint : unit -> t

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end = struct
  module Key = struct
    type t = < >
    let compare = compare
  end

  type t = Key.t
  let mint () = object end

  module Set = Set.Make(Key)
  module Map = Map.Make(Key)
end

module Make (Job : sig type id end) = struct
  type state =
    | Blocked
    | Active of Output.active
    | Pass
    | Fail of string

  type t = {
    id : Id.t;
    bind : t option;
    ty : metadata_ty;
    state : state;
  }
  and metadata_ty =
    | Constant of string option
    | Map_input of { source : t; info : (string, [`Blocked | `Empty_list]) result }
    | Opt_input of { source : t; info : [`Blocked | `Selected | `Not_selected] }
    | State of { source : t; hidden : bool }
    | Catch of { source : t; hidden : bool }
    | Map_failed of t      (* In [map f t], [f] raised an exception. *)
    | Bind of t * string
    | Bind_input of {x : t; info : string; id : Job.id option}
    | Pair of t * t
    | Gate_on of { ctrl : t; value : t }
    | List_map of { items : t; fn : t }
    | Option_map of { item : t; fn : t }

  type env = t option

  let make ~env ty state =
    { id = Id.mint (); ty; bind = env; state }

  let return ~env label =
    make ~env (Constant label) Pass

  let map_input ~env source info =
    make ~env (Map_input {source; info})
      (match info with
       | Ok _ -> Pass
       | Error (`Blocked | `Empty_list) -> Blocked)

  let option_input ~env source info =
    make ~env (Opt_input {source; info})
      (match info with
       | `Selected -> Pass
       | `Blocked | `Not_selected -> Blocked)

  let fail ~env msg =
    make ~env (Constant None) (Fail msg)

  let map_failed ~env t msg =
    make ~env (Map_failed t) (Fail msg)

  let state ~env ~hidden t =
    make ~env (State { source = t; hidden }) Pass

  let catch ~env ~hidden t =
    let state =
      match t.state with
      | Fail _ -> Pass
      | _  -> t.state
    in
    make ~env (Catch { source = t; hidden }) state

  let active ~env a =
    make ~env (Constant None) (Active a)

  let of_output ~env = function
    | Ok _ -> return ~env None
    | Error `Msg m -> fail ~env m
    | Error (`Active a) -> active ~env a

  let ( =? ) a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> a.id = b.id
    | _ -> false

  let simplify x =
    match x.ty, x.bind with
    | Constant None, Some c -> c
    | _ -> x

  let pair_state a b =
    match a.state, b.state with
    | _, Fail m
    | Fail m, _ -> Fail m
    | _, (Blocked | Active _)
    | (Blocked | Active _), _ -> Blocked
    | Pass, Pass -> Pass

  let pair ~env a b =
    let a = simplify a in
    let b = simplify b in
    let single_context =
      a.bind =? b.bind && b.bind =? env
    in
    match single_context, a.ty, b.ty with
    | true, Constant None, _ -> b
    | true, _, Constant None -> a
    | _ ->
      make ~env (Pair (a, b)) (pair_state a b)

  let bind ~env ?(info="") x state =
    match info, env with
    | "", Some bind -> bind
    | _ ->
      let x = simplify x in
      let ty = Bind (x, info) in
      let state =
        match x.state with
        | Blocked | Active _ | Fail _ -> Blocked
        | Pass -> state
      in
      make ~env ty state

  let bind_input ~env ~info ?id x state =
    let x = simplify x in
    let ty = Bind_input {x; info; id} in
    let state =
      match x.state with
      | Blocked | Active _ | Fail _ -> Blocked
      | Pass -> state
    in
    make ~env ty state

  let list_map ~env ~f items =
    make ~env (List_map { items; fn = f }) items.state

  let option_map ~env ~f item =
    make ~env (Option_map { item; fn = f }) item.state

  let gate ~env ~on:ctrl value =
    let ctrl = simplify ctrl in
    let value = simplify value in
    make ~env (Gate_on { ctrl; value }) (pair_state ctrl value)

  let pp f x =
    let seen = ref Id.Set.empty in
    let rec aux f md =
      if Id.Set.mem md.id !seen then
        Fmt.string f "..."
      else (
        seen := Id.Set.add md.id !seen;
        match md.ty with
        | Constant None ->
          begin match md.bind with
            | Some ctx -> aux f ctx
            | None -> Fmt.string f (if md.state = Blocked then "(input)" else "(const)")
          end
        | Constant (Some l) -> Fmt.string f l
        | Map_input { source = _; info = Ok label } -> Fmt.string f label
        | Map_input { source = _; info = Error `Blocked } -> Fmt.string f "(blocked)"
        | Map_input { source = _; info = Error `Empty_list } -> Fmt.string f "(empty list)"
        | Opt_input { source; info = _ } -> Fmt.pf f "[%a]" aux source
        | Bind (x, name) -> Fmt.pf f "%a@;>>=@;%s" aux x name
        | Bind_input {x; info; id = _} -> Fmt.pf f "%a@;>>=@;%s" aux x info
        | Pair (x, y) -> Fmt.pf f "@[<v>@[%a@]@,||@,@[%a@]@]" aux x aux y
        | Gate_on { ctrl; value } -> Fmt.pf f "%a@;>>@;gate (@[%a@])" aux value aux ctrl
        | List_map { items; fn } -> Fmt.pf f "%a@;>>@;list_map (@[%a@])" aux items aux fn
        | Option_map { item; fn } -> Fmt.pf f "%a@;>>@;option_map (@[%a@])" aux item aux fn
        | State x -> Fmt.pf f "state(@[%a@])" aux x.source
        | Catch x -> Fmt.pf f "catch(@[%a@])" aux x.source
        | Map_failed x -> aux f x
      )
    in
    aux f x

  module Node_set = Set.Make(struct type t = int let compare = compare end)

  module Out_node = struct
    (* Information about one node in the OCurrent graph. *)

    type t = {
      outputs : Node_set.t;
      (* Normally, this will be a singleton set containing just the node being
         considered. However, sometimes we choose to hide nodes. For example,
         if we have a node C representing the pair of A and B then we don't bother
         adding a dot node for C. Instead, C is represented by an `Out_node` with
         `outputs = {A, B}`. Anything that takes input from C will instead take
         input directly from both A and B. *)

      trans : Node_set.t;
      (* The set of nodes which must be resolved for this node to be resolved.
         For example, in the graph `A -> B -> C`:

         trans(A) = {A}
         trans(B) = {A, B}
         trans(C) = {A, B, C}

         This is used to hide edges that are implied by other edges, to simplify
         the output. *)
    }

    let empty = {
      outputs = Node_set.empty;
      trans = Node_set.empty;
    }

    let is_empty t = Node_set.is_empty t.outputs

    (* The union of A and B's outputs and transitive dependencies,
       except that we remove outputs that are already dependencies
       of the other branch. *)
    let union a b =
      let outputs =
        Node_set.union
          (Node_set.filter (fun x -> not (Node_set.mem x b.trans)) a.outputs)
          (Node_set.filter (fun x -> not (Node_set.mem x a.trans)) b.outputs)
      in
      let outputs =
        if Node_set.is_empty outputs then a.outputs
        else outputs
      in {
        outputs;
        trans = Node_set.union a.trans b.trans;
      }

    (* Connect [t]'s outputs using [make_edge] to connect to something. *)
    let connect make_edge t =
      Node_set.iter make_edge t.outputs

    (* An ordinary node, represented by a single box in the diagram. *)
    let singleton ~deps i = {
      outputs = Node_set.singleton i;
      trans = Node_set.add i deps;
    }
  end

  let pp_dot ~url f x =
    let next = ref 0 in
    let seen : Out_node.t Id.Map.t ref = ref Id.Map.empty in
    let pending_edges = ref [] in
    let edge_to ?style ?color b a = pending_edges := (style, color, a, b) :: !pending_edges in
    let flush_pending () =
      !pending_edges |> List.iter (fun (style, color, a, b) -> Dot.edge f ?style ?color a b);
      pending_edges := []
    in
    let rec aux md =
      match Id.Map.find_opt md.id !seen with
      | Some x -> x
      | None ->
        let i = !next in
        incr next;
        let ctx =
          match md.bind with
          | None -> Out_node.empty
          | Some c -> aux c
        in
        let bg =
          match md.state with
          | Blocked -> "#d3d3d3"
          | Active `Ready -> "#ffff00"
          | Active `Running -> "#ffa500"
          | Pass -> "#90ee90"
          | Fail _ -> "#ff4500"
        in
        let tooltip =
          match md.state with
          | Fail msg -> Some msg
          | _ -> None
        in
        let node ?id =
          let url = match id with None -> None | Some id -> url id in
          Dot.node ~style:"filled" ~bg ?tooltip ?url f in
        let outputs =
          match md.ty with
          | Constant (Some l) -> node i l; Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None when Out_node.is_empty ctx ->
            node i (if md.state = Blocked then "(input)" else "(const)");
            Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None -> ctx
          | Map_input { source; info } ->
            let label =
              match info with
              | Ok l -> l
              | Error `Blocked -> "(each item)"
              | Error `Empty_list -> "(empty list)"
            in
            node i label;
            let source = aux source in
            Out_node.connect (edge_to i) source;
            let deps = Node_set.union source.Out_node.trans ctx.Out_node.trans in
            Out_node.singleton ~deps i
          | Opt_input { source; info = _ } ->
            aux source
          | Bind (x, name) ->
            let inputs =
              match x.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            node i name;
            let all_inputs = Out_node.union inputs ctx in
            Out_node.connect (edge_to i) all_inputs;
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Bind_input {x; info; id} ->
            let inputs =
              match x.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            node ?id i info;
            let all_inputs = Out_node.union inputs ctx in
            Out_node.connect (edge_to i) all_inputs;
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Pair (x, y) ->
            Out_node.union (aux x) (aux y) |> Out_node.union ctx
          | Gate_on { ctrl; value } ->
            let ctrls = aux ctrl in
            let values = aux value in
            node i "" ~shape:"circle";
            ctrls |> Out_node.connect (edge_to i ~style:"dashed");
            let data_inputs = Out_node.union values ctx in
            Out_node.connect (edge_to i) data_inputs;
            let deps = Node_set.(union ctrls.trans data_inputs.trans) in
            Out_node.singleton ~deps i
          | Catch { source; hidden = true }
          | State { source; hidden = true } ->
            aux source
          | State { source; hidden = false } ->
            let inputs = aux source in
            node i "state";
            Out_node.connect (edge_to i) inputs;
            (* Because a state node will be ready even when its inputs aren't, we shouldn't
               remove dependencies just because they're also dependencies of a state node.
               e.g. setting a GitHub status depends on knowing which commit is to be tested
               and the state of the build. We can know the state of the build (pending) without
               yet knowing the commit. So the set_state node can't run even though its
               state input is ready and transitively depends on knowing the commit. *)
            Out_node.singleton ~deps:Node_set.empty i
          | Catch { source; hidden = false } ->
            let inputs = aux source in
            node i "catch";
            let all_inputs = Out_node.union inputs ctx in
            Out_node.connect (edge_to i) all_inputs;
            Out_node.singleton ~deps:all_inputs.trans i
          | Map_failed x ->
            (* Normally, we don't show separate boxes for map functions.
               But we do if one fails. *)
            let inputs = aux x in
            node i "map";
            let all_inputs = Out_node.union inputs ctx in
            Out_node.connect (edge_to i) all_inputs;
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | List_map { items; fn } ->
            ignore (aux items);
            Dot.begin_cluster f i;
            let outputs = aux fn in
            Dot.end_cluster f;
            outputs
          | Option_map { item; fn } ->
            ignore (aux item);
            Dot.begin_cluster f i;
            Dot.pp_option f ("style", "dotted");
            let outputs = aux fn in
            Dot.end_cluster f;
            outputs
        in
        seen := Id.Map.add md.id outputs !seen;
        outputs
    in
    Fmt.pf f "@[<v2>digraph pipeline {@,\
                node [shape=\"box\"]@,\
                rankdir=LR@,";
    let _ = aux x in
    flush_pending ();
    Fmt.pf f "}@]@."

  (* This is similar to [pp_dot], except that for each call to [node] we call [count] instead. *)
  let stats x =
    let seen : Out_node.t Id.Map.t ref = ref Id.Map.empty in
    let next = ref 0 in
    let ok = ref 0 in
    let ready = ref 0 in
    let running = ref 0 in
    let failed = ref 0 in
    let blocked = ref 0 in
    let rec aux md =
      match Id.Map.find_opt md.id !seen with
      | Some x -> x
      | None ->
        let i = !next in
        incr next;
        let ctx =
          match md.bind with
          | None -> Out_node.empty
          | Some c -> aux c
        in
        let count () =
          match md.state with
          | Pass -> incr ok
          | Blocked -> incr blocked
          | Active `Ready -> incr ready
          | Active `Running -> incr running
          | Fail _ -> incr failed
        in
        let outputs =
          match md.ty with
          | Constant (Some _) -> count (); Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None when Out_node.is_empty ctx ->
            count ();
            Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None -> ctx
          | Map_input { source; info = _ } ->
            count ();
            let source = aux source in
            let deps = Node_set.union source.Out_node.trans ctx.Out_node.trans in
            Out_node.singleton ~deps i
          | Opt_input { source; info = _ } -> aux source
          | Bind (x, _) ->
            let inputs =
              match x.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            count ();
            let all_inputs = Out_node.union inputs ctx in
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Bind_input {x; info = _; id = _} ->
            let inputs =
              match x.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            count ();
            let all_inputs = Out_node.union inputs ctx in
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Pair (x, y) ->
            Out_node.union (aux x) (aux y) |> Out_node.union ctx
          | Gate_on { ctrl; value } ->
            count ();
            let ctrls = aux ctrl in
            let values = aux value in
            let data_inputs = Out_node.union values ctx in
            let deps = Node_set.(union ctrls.trans data_inputs.trans) in
            Out_node.singleton ~deps i
          | State { source; hidden } ->
            let _ : Out_node.t = aux source in
            if not hidden then count ();
            Out_node.singleton ~deps:Node_set.empty i
          | Catch { source; hidden } ->
            let inputs = aux source in
            if not hidden then count ();
            let all_inputs = Out_node.union inputs ctx in
            Out_node.singleton ~deps:all_inputs.trans i
          | Map_failed x ->
            let inputs = aux x in
            count ();
            let all_inputs = Out_node.union inputs ctx in
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | List_map { items; fn } ->
            ignore (aux items);
            aux fn
          | Option_map { item; fn } ->
            ignore (aux item);
            aux fn
        in
        seen := Id.Map.add md.id outputs !seen;
        outputs
    in
    ignore (aux x);
    { S.ok = !ok; ready = !ready; running = !running; failed = !failed; blocked = !blocked  }

  let booting =
    active ~env:None `Running

  let rec job_id t =
    match t.ty with
    | Bind_input i -> i.id
    | Option_map x -> job_id x.fn
    | _ -> None
end
