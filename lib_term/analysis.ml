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
    | Active
    | Pass
    | Fail

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

  let return ~env label =
    make ~env (Constant label) Pass

  let fail ~env () =
    make ~env (Constant None) Fail

  let state ~env t =
    make ~env (State t) Pass

  let catch ~env t =
    let state = if t.state = Fail then Pass else t.state in
    make ~env (Catch t) state

  let pending ~env () =
    make ~env (Constant None) Active

  let of_output ~env = function
    | Ok _ -> return ~env None
    | Error `Msg _ -> fail ~env ()
    | Error `Pending -> pending ~env ()

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
    | _, Fail
    | Fail, _ -> Fail
    | _, (Blocked | Active)
    | (Blocked | Active), _ -> Blocked
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
        | Blocked | Active | Fail -> Blocked
        | Pass -> state
      in
      make ~env:parent ty state

  let bind_input ~env:parent ~info x state =
    let x = simplify x in
    let ty = Bind_input {x; info; id = None} in
    let state =
      match x.state with
      | Blocked | Active | Fail -> Blocked
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
            | None -> Fmt.string f "(const)"
          end
        | Constant (Some l) -> Fmt.string f l
        | Bind (x, name) -> Fmt.pf f "%a@;>>=@;%s" aux x name
        | Bind_input {x; info; id = _} -> Fmt.pf f "%a@;>>=@;%s" aux x info
        | Pair (x, y) -> Fmt.pf f "@[<v>@[%a@]@,||@,@[%a@]@]" aux x aux y
        | Gate_on { ctrl; value } -> Fmt.pf f "%a@;>>@;gate (@[%a@])" aux value aux ctrl
        | List_map { items; fn } -> Fmt.pf f "%a@;>>@;list_map (@[%a@])" aux items aux fn
        | State x -> Fmt.pf f "state(@[%a@])" aux x
        | Catch x -> Fmt.pf f "catch(@[%a@])" aux x
      )
    in
    aux f x

  type out_node = {
    i : int;
    outputs : int list;
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
          | None -> []
          | Some c -> aux c
        in
        let bg =
          match md.state with
          | Blocked -> "#d3d3d3"
          | Active -> "#ffa500"
          | Pass -> "#90ee90"
          | Fail -> "#ff4500"
        in
        let node ?id =
          let url = match id with None -> None | Some id -> url id in
          Dot.node ~style:"filled" ~bg ?url f in
        let outputs =
          match md.ty with
          | Constant (Some l) -> node i l; [i]
          | Constant None when ctx = [] -> node i "(const)"; [i]
          | Constant None -> ctx
          | Bind (x, name) ->
            let inputs =
              match x.ty with
              | Constant None -> []
              | _ -> aux x
            in
            node i name;
            inputs |> List.iter (fun input -> input ==> i);
            ctx |> List.iter (fun input -> input ==> i);
            [i]
          | Bind_input {x; info; id} ->
            let inputs =
              match x.ty with
              | Constant None -> []
              | _ -> aux x
            in
            node ?id i info;
            inputs |> List.iter (fun input -> input ==> i);
            ctx |> List.iter (fun input -> input ==> i);
            [i]
          | Pair (x, y) ->
            aux x @ aux y @ ctx
          | Gate_on { ctrl; value } ->
            let ctrls = aux ctrl in
            let values = aux value in
            node i "" ~shape:"circle";
            ctrls |> List.iter (fun input -> edge input i ~style:"dashed");
            values |> List.iter (fun input -> input ==> i);
            ctx |> List.iter (fun input -> input ==> i);
            [i]
          | State x ->
            let inputs = aux x in
            node i "state";
            inputs |> List.iter (fun input -> input ==> i);
            [i]
          | Catch x ->
            let inputs = aux x in
            node i "catch";
            inputs |> List.iter (fun input -> input ==> i);
            [i]
          | List_map { items; fn } ->
            let items = aux items in
            Dot.begin_cluster f i;
            Dot.node f ~shape:"none" i "map";
            ctx |> List.iter (fun input -> input ==> i);
            let outputs = aux fn in
            Dot.end_cluster f;
            items |> List.iter (fun input -> edge input i);
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
    pending ~env ()
end
