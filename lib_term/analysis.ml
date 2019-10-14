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
    (* For bind, we must create the node before knowing the state *)
    mutable ty : metadata_ty;
    mutable state : state;
  }
  and metadata_ty =
    | Constant of string option
    | State of t
    | Catch of t
    | Map_failed of t      (* In [map f t], [f] raised an exception. *)
    | Bind of t * string
    | Bind_input of {x : t; info : string; id : Job.id option}
    | Pair of t * t
    | Gate_on of { ctrl : t; value : t }
    | List_map of { items : t; fn : t }

  type env = {
    next : int ref;
    bind : t option;
  }

  let make_env () =
    { next = ref 0; bind = None }

  let with_bind bind env = { env with bind = Some bind }

  let make ~env ty state =
    incr env.next;
    { id = Id.mint (); ty; bind = env.bind; state }

  let blocked ~env () =
    make ~env (Constant None) Blocked

  let return ~env label =
    make ~env (Constant label) Pass

  let fail ~env msg =
    make ~env (Constant None) (Fail msg)

  let map_failed ~env t msg =
    make ~env (Map_failed t) (Fail msg)

  let state ~env t =
    make ~env (State t) Pass

  let catch ~env t =
    let state =
      match t.state with
      | Fail _ -> Pass
      | _  -> t.state
    in
    make ~env (Catch t) state

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
      a.bind =? b.bind && b.bind =? env.bind
    in
    match single_context, a.ty, b.ty with
    | true, Constant None, _ -> b
    | true, _, Constant None -> a
    | _ ->
      make ~env (Pair (a, b)) (pair_state a b)

  let bind ~env:parent ?(info="") x state =
    match info, parent with
    | "", { bind = Some bind; _ } -> bind
    | _ ->
      let x = simplify x in
      let ty = Bind (x, info) in
      let state =
        match x.state with
        | Blocked | Active _ | Fail _ -> Blocked
        | Pass -> state
      in
      make ~env:parent ty state

  let bind_input ~env:parent ~info x state =
    let x = simplify x in
    let ty = Bind_input {x; info; id = None} in
    let state =
      match x.state with
      | Blocked | Active _ | Fail _ -> Blocked
      | Pass -> state
    in
    make ~env:parent ty state

  let list_map ~env ~f items =
    make ~env (List_map { items; fn = f }) items.state

  let gate ~env ~on:ctrl value =
    let ctrl = simplify ctrl in
    let value = simplify value in
    make ~env (Gate_on { ctrl; value }) (pair_state ctrl value)

  let set_state t ?id state =
    t.state <- state;
    match t.ty, id with
    | Bind_input {x; info; id = None}, Some _ -> t.ty <- Bind_input {x; info; id}
    | _, None -> ()
    | _ -> assert false

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
        | Bind (x, name) -> Fmt.pf f "%a@;>>=@;%s" aux x name
        | Bind_input {x; info; id = _} -> Fmt.pf f "%a@;>>=@;%s" aux x info
        | Pair (x, y) -> Fmt.pf f "@[<v>@[%a@]@,||@,@[%a@]@]" aux x aux y
        | Gate_on { ctrl; value } -> Fmt.pf f "%a@;>>@;gate (@[%a@])" aux value aux ctrl
        | List_map { items; fn } -> Fmt.pf f "%a@;>>@;list_map (@[%a@])" aux items aux fn
        | State x -> Fmt.pf f "state(@[%a@])" aux x
        | Catch x -> Fmt.pf f "catch(@[%a@])" aux x
        | Map_failed x -> aux f x
      )
    in
    aux f x

  module Node_set = Set.Make(struct type t = int let compare = compare end)

  type out_node = {
    i : int;
    outputs : Node_set.t;
  }

  let pp_dot ~url f x =
    let next = ref 0 in
    let seen : out_node Id.Map.t ref = ref Id.Map.empty in
    let edge ?style ?color a b = Dot.edge f ?style ?color a b in
    let ( ==> ) a b = edge a b in
    let rec aux md =
      match Id.Map.find_opt md.id !seen with
      | Some x -> x.outputs
      | None ->
        let i = !next in
        incr next;
        let ctx =
          match md.bind with
          | None -> Node_set.empty
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
          | Constant (Some l) -> node i l; Node_set.singleton i
          | Constant None when Node_set.is_empty ctx ->
            node i (if md.state = Blocked then "(input)" else "(const)");
            Node_set.singleton i
          | Constant None -> ctx
          | Bind (x, name) ->
            let inputs =
              match x.ty with
              | Constant None -> Node_set.empty
              | _ -> aux x
            in
            node i name;
            inputs |> Node_set.iter (fun input -> input ==> i);
            ctx |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | Bind_input {x; info; id} ->
            let inputs =
              match x.ty with
              | Constant None -> Node_set.empty
              | _ -> aux x
            in
            node ?id i info;
            inputs |> Node_set.iter (fun input -> input ==> i);
            ctx |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | Pair (x, y) ->
            Node_set.union (aux x) (aux y) |> Node_set.union ctx
          | Gate_on { ctrl; value } ->
            let ctrls = aux ctrl in
            let values = aux value in
            node i "" ~shape:"circle";
            ctrls |> Node_set.iter (fun input -> edge input i ~style:"dashed");
            values |> Node_set.iter (fun input -> input ==> i);
            ctx |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | State x ->
            let inputs = aux x in
            node i "state";
            inputs |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | Catch x ->
            let inputs = aux x in
            node i "catch";
            inputs |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | Map_failed x ->
            (* Normally, we don't show separate boxes for map functions.
               But we do if one fails. *)
            let inputs = aux x in
            node i "map";
            inputs |> Node_set.iter (fun input -> input ==> i);
            Node_set.singleton i
          | List_map { items; fn } ->
            let items = aux items in
            Dot.begin_cluster f i;
            Dot.node f ~shape:"none" i "map";
            ctx |> Node_set.iter (fun input -> input ==> i);
            let outputs = aux fn in
            Dot.end_cluster f;
            items |> Node_set.iter (fun input -> edge input i);
            outputs
        in
        seen := Id.Map.add md.id { i; outputs } !seen;
        outputs
    in
    Fmt.pf f "@[<v2>digraph pipeline {@,\
                node [shape=\"box\"]@,\
                rankdir=LR@,";
    let _ = aux x in
    Fmt.pf f "}@]@."

  let booting =
    let env = make_env () in
    active ~env `Running

  let job_id t =
    match t.ty with
    | Bind_input i -> i.id
    | _ -> None
end
