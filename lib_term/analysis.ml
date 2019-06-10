module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

type state =
  | Blocked
  | Active
  | Pass
  | Fail

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

type t = {
  id : Id.t;
  bind : t option;
  ty : metadata_ty;
  mutable state : state;  (* For bind, we must create the node before knowing the state *)
}
and metadata_ty =
  | Constant
  | Prim of string
  | Bind of t * string
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

let return ~env () =
  make ~env Constant Pass

let fail ~env () =
  make ~env Constant Fail

let pending ~env () =
  make ~env Constant Active

let of_output ~env = function
  | Ok _ -> return ~env ()
  | Error `Msg _ -> fail ~env ()
  | Error `Pending -> pending ~env ()

let ( =? ) a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> a.id = b.id
  | _ -> false

let simplify x =
  match x.ty, x.bind with
  | Constant, Some c -> c
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
  | true, Constant, _ -> b
  | true, _, Constant -> a
  | _ ->
    make ~env (Pair (a, b)) (pair_state a b)

let bind ~env:parent ~name x state =
  let x = simplify x in
  let ty =
    match x.ty with
    | Constant -> Prim name
    | _ -> Bind (x, name)
  in
  let state =
    match x.state with
    | Blocked | Active -> Blocked
    | Pass | Fail -> state
  in
  make ~env:parent ty state

let list_map ~env ~f items =
  make ~env (List_map { items; fn = f }) items.state

let gate ~env ~on:ctrl value =
  let ctrl = simplify ctrl in
  let value = simplify value in
  make ~env (Gate_on { ctrl; value }) (pair_state ctrl value)

let set_state t state =
  t.state <- state

let pp f x =
  let seen = ref Id.Set.empty in
  let rec aux f md =
    if Id.Set.mem md.id !seen then
      Fmt.string f "..."
    else (
      seen := Id.Set.add md.id !seen;
      match md.ty with
      | Constant ->
        begin match md.bind with
          | Some ctx -> aux f ctx
          | None -> Fmt.string f "(const)"
        end
      | Prim name -> Fmt.string f name
      | Bind (x, name) -> Fmt.pf f "%a >>= %s" aux x name
      | Pair (x, y) -> Fmt.pf f "(%a, %a)" aux x aux y
      | Gate_on { ctrl; value } -> Fmt.pf f "%a; %a" aux ctrl aux value
      | List_map { items; fn } -> Fmt.pf f "%a >> list_map (%a)" aux items aux fn
    )
  in
  aux f x

type out_node = {
  i : int;
  outputs : int list;
}

let pp_dot f x =
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
        | Blocked -> "lightgray"
        | Active -> "orange"
        | Pass -> "lightgreen"
        | Fail -> "orangered"
      in
      let node = Dot.node ~style:"filled" ~bg f in
      let outputs =
        match md.ty with
        | Constant when ctx = [] -> node i "(const)"; [i]
        | Constant -> ctx
        | Prim name -> node i name; i :: ctx
        | Bind (x, name) ->
          let inputs = aux x in
          node i name;
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
