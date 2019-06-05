module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

type state =
  | Blocked
  | Active
  | Pass
  | Fail

type t = {
  i : int;
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

let next =
  let n = ref 0 in
  fun () ->
    let r = !n in
    n := r + 1;
    r

let make ~bind ty state =
  let i = next () in
  { i; ty; bind; state }

let return ~bind () =
  make ~bind Constant Pass

let fail ~bind () =
  make ~bind Constant Fail

let pending ~bind () =
  make ~bind Constant Active

let ( =? ) a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> a.i = b.i
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

let pair ~bind a b =
  let a = simplify a in
  let b = simplify b in
  let single_context =
    a.bind =? b.bind && b.bind =? bind
  in
  match single_context, a.ty, b.ty with
  | true, Constant, _ -> b
  | true, _, Constant -> a
  | _ ->
    make ~bind (Pair (a, b)) (pair_state a b)

let bind ~bind:parent ~name x state =
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
  make ~bind:parent ty state

let list_map ~bind ~f items =
  make ~bind (List_map { items; fn = f }) items.state

let gate ~bind ~on:ctrl value =
  let ctrl = simplify ctrl in
  let value = simplify value in
  make ~bind (Gate_on { ctrl; value }) (pair_state ctrl value)

let set_state t state =
  t.state <- state

let pp f x =
  let seen = ref IntSet.empty in
  let rec aux f md =
    if IntSet.mem md.i !seen then
      Fmt.string f "..."
    else (
      seen := IntSet.add md.i !seen;
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

let pp_dot f x =
  let seen = ref IntMap.empty in
  let edge ?style ?color a b = Dot.edge f ?style ?color a.i b.i in
  let ( ==> ) a b = edge a b in
  let rec aux md =
    match IntMap.find_opt md.i !seen with
    | Some x -> x
    | None ->
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
        | Constant when ctx = [] -> node md.i "(const)"; [md]
        | Constant -> ctx
        | Prim name -> node md.i name; md :: ctx
        | Bind (x, name) ->
          let inputs = aux x in
          node md.i name;
          inputs |> List.iter (fun input -> input ==> md);
          ctx |> List.iter (fun input -> input ==> md);
          [md]
        | Pair (x, y) ->
          aux x @ aux y @ ctx
        | Gate_on { ctrl; value } ->
          let ctrls = aux ctrl in
          let values = aux value in
          node md.i "" ~shape:"circle";
          ctrls |> List.iter (fun input -> edge input md ~style:"dashed");
          values |> List.iter (fun input -> input ==> md);
          ctx |> List.iter (fun input -> input ==> md);
          [md]
        | List_map { items; fn } ->
          let items = aux items in
          node md.i "map";
          items |> List.iter (fun input -> edge input md);
          ctx |> List.iter (fun input -> input ==> md);
          let outputs = aux fn in
          outputs |> List.iter (fun outputs -> edge outputs md);  (* TODO *)
          [md]
      in
      seen := IntMap.add md.i outputs !seen;
      outputs
  in
  Fmt.pf f "@[<v2>digraph pipeline {@,\
              node [shape=\"box\"]@,\
              rankdir=LR@,";
  let _ = aux x in
  Fmt.pf f "}@]@."
