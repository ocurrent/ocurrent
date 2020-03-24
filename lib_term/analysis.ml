module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

module Make (Meta : sig type job_id end) = struct
  module Node = Node.Make(Meta)
  open Node

  let pp f x =
    let seen = ref Id.Set.empty in
    let rec aux f t =
      let Term t = t in
      if Id.Set.mem t.id !seen then
        Fmt.string f "..."
      else (
        seen := Id.Set.add t.id !seen;
        match t.ty with
        | Constant None ->
          begin match t.bind with
            | Some ctx -> aux f ctx
            | None ->
              match Current_incr.observe t.v with
              | Error (_, `Active _) -> Fmt.string f "(input)"
              | _ -> Fmt.string f "(const)"
          end
        | Constant (Some l) -> Fmt.string f l
        | Map_input { source = _; info = Ok label } -> Fmt.string f label
        | Map_input { source = _; info = Error `Blocked } -> Fmt.string f "(blocked)"
        | Map_input { source = _; info = Error `Empty_list } -> Fmt.string f "(empty list)"
        | Opt_input { source } -> Fmt.pf f "[%a]" aux source
        | Bind_in (x, name) -> Fmt.pf f "%a@;>>=@;%s" aux x name
        | Bind_out x -> aux f (Current_incr.observe x)
        | Primitive {x; info; meta = _ } -> Fmt.pf f "%a@;>>=@;%s" aux x info
        | Pair (x, y) -> Fmt.pf f "@[<v>@[%a@]@,||@,@[%a@]@]" aux x aux y
        | Gate_on { ctrl; value } -> Fmt.pf f "%a@;>>@;gate (@[%a@])" aux value aux ctrl
        | List_map { items; output } -> Fmt.pf f "%a@;>>@;list_map (@[%a@])" aux items aux (Current_incr.observe output)
        | Option_map { item; output } -> Fmt.pf f "%a@;>>@;option_map (@[%a@])" aux item aux (Current_incr.observe output)
        | State x -> Fmt.pf f "state(@[%a@])" aux x.source
        | Catch x -> Fmt.pf f "catch(@[%a@])" aux x.source
        | Map x -> aux f x
      )
    in
    aux f (Term x)

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
    let rec aux (Term t) =
      match Id.Map.find_opt t.id !seen with
      | Some x -> x
      | None ->
        let i = !next in
        incr next;
        let ctx =
          match t.bind with
          | None -> Out_node.empty
          | Some c -> aux c
        in
        let v = Current_incr.observe t.v in
        let error_from_self =
          match v with
          | Error (id, _) -> Id.equal id t.id
          | Ok _ -> false
        in
        let bg =
          match v with
          | Ok _ -> "#90ee90"
          | Error _ when not error_from_self -> "#d3d3d3" (* Blocked *)
          | Error (_, `Active `Ready) -> "#ffff00"
          | Error (_, `Active `Running) -> "#ffa500"
          | Error (_, `Msg _) -> "#ff4500"
        in
        let tooltip =
          match v with
          | Error (_, `Msg msg) when error_from_self -> Some msg
          | _ -> None
        in
        let node ?id =
          let url = match id with None -> None | Some id -> url id in
          Dot.node ~style:"filled" ~bg ?tooltip ?url f in
        let outputs =
          match t.ty with
          | Constant (Some l) -> node i l; Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None when Out_node.is_empty ctx ->
            if Result.is_ok v then ctx
            else (
              node i (if error_from_self then "(const)" else "(input)");
              Out_node.singleton ~deps:ctx.Out_node.trans i
            )
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
          | Opt_input { source } ->
            aux source
          | Bind_in (x, name) ->
            let inputs =
              match t.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            node i name;
            let all_inputs = Out_node.union inputs ctx in
            Out_node.connect (edge_to i) all_inputs;
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Bind_out x -> aux (Current_incr.observe x)
          | Primitive {x; info; meta} ->
            let inputs =
              match x with
              | Term { ty = Constant None; _ } -> Out_node.empty
              | _ -> aux x
            in
            let id = Current_incr.observe meta in
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
          | Map x ->
            let inputs = aux x in
            begin match v with
              | Error (_, `Msg _) when error_from_self ->
                (* Normally, we don't show separate boxes for map functions.
                   But we do if one fails. *)
                node i "map";
                let all_inputs = Out_node.union inputs ctx in
                Out_node.connect (edge_to i) all_inputs;
                Out_node.singleton ~deps:all_inputs.Out_node.trans i
              | _ ->
                aux x
            end
          | List_map { items; output } ->
            ignore (aux items);
            Dot.begin_cluster f i;
            let outputs = aux (Current_incr.observe output) in
            Dot.end_cluster f;
            outputs
          | Option_map { item; output } ->
            ignore (aux item);
            Dot.begin_cluster f i;
            Dot.pp_option f ("style", "dotted");
            let outputs = aux (Current_incr.observe output) in
            Dot.end_cluster f;
            outputs
        in
        seen := Id.Map.add t.id outputs !seen;
        outputs
    in
    Fmt.pf f "@[<v2>digraph pipeline {@,\
              node [shape=\"box\"]@,\
              rankdir=LR@,";
    let _ = aux (Term x) in
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
    let rec aux (Term t) =
      match Id.Map.find_opt t.id !seen with
      | Some x -> x
      | None ->
        let i = !next in
        incr next;
        let ctx =
          match t.bind with
          | None -> Out_node.empty
          | Some c -> aux c
        in
        let v = Current_incr.observe t.v in
        let error_from_self =
          match v with
          | Error (id, _) -> Id.equal id t.id
          | Ok _ -> false
        in
        let count () =
          match v with
          | Ok _ -> incr ok
          | _ when not error_from_self -> incr blocked
          | Error (_, `Active `Ready) -> incr ready
          | Error (_, `Active `Running) -> incr running
          | Error (_, `Msg _) -> incr failed
        in
        let outputs =
          match t.ty with
          | Constant (Some _) -> count (); Out_node.singleton ~deps:ctx.Out_node.trans i
          | Constant None when Out_node.is_empty ctx ->
            if Result.is_ok v then ctx
            else (
              count ();
              Out_node.singleton ~deps:ctx.Out_node.trans i
            )
          | Constant None -> ctx
          | Map_input { source; info = _ } ->
            count ();
            let source = aux source in
            let deps = Node_set.union source.Out_node.trans ctx.Out_node.trans in
            Out_node.singleton ~deps i
          | Opt_input { source } ->
            aux source
          | Bind_in (x, _name) ->
            let inputs =
              match t.ty with
              | Constant None -> Out_node.empty
              | _ -> aux x
            in
            count ();
            let all_inputs = Out_node.union inputs ctx in
            Out_node.singleton ~deps:all_inputs.Out_node.trans i
          | Bind_out x -> aux (Current_incr.observe x)
          | Primitive {x; info = _; meta = _} ->
            let inputs =
              match x with
              | Term { ty = Constant None; _ } -> Out_node.empty
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
          | Map x ->
            let inputs = aux x in
            let all_inputs = Out_node.union inputs ctx in
            begin match v with
              | Error (_, `Msg _) when error_from_self ->
                count ();
                Out_node.singleton ~deps:all_inputs.Out_node.trans i
              | _ ->
                aux x
            end
          | List_map { items; output } ->
            ignore (aux items);
            aux (Current_incr.observe output)
          | Option_map { item; output } ->
            ignore (aux item);
            aux (Current_incr.observe output)
        in
        seen := Id.Map.add t.id outputs !seen;
        outputs
    in
    ignore (aux (Term x));
    { S.ok = !ok; ready = !ready; running = !running; failed = !failed; blocked = !blocked  }
end
