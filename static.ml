module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)

type t = {
  i : int;
  context : t option;
  ty : metadata_ty;
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

let context = ref None

let make ty =
  let i = next () in
  { i; ty; context = !context }

let return () =
  make Constant

let fail () =
  make Constant

let pending =
  make Constant

let ( =? ) a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> a.i = b.i
  | _ -> false

let simplify x =
  match x.ty, x.context with
  | Constant, Some c -> c
  | _ -> x

let pair a b =
  let a = simplify a in
  let b = simplify b in
  let single_context =
    a.context =? b.context && b.context =? !context
  in
  match single_context, a.ty, b.ty with
  | true, Constant, _ -> b
  | true, _, Constant -> a
  | _ -> make @@ Pair (a, b)

let bind ~name x =
  let x = simplify x in
  make @@ match x.ty with
  | Constant -> Prim name
  | _ -> Bind (x, name)

let list_map ~f items =
  make (List_map { items; fn = f })

let gate ~on:ctrl value =
  let ctrl = simplify ctrl in
  let value = simplify value in
  make (Gate_on { ctrl; value })

let with_context md f x =
  let md = { i = next (); ty = md.ty; context = !context } in
  context := Some md;
  try
    let r = f x in
    context := md.context;
    r
  with ex ->
    context := md.context;
    raise ex

let pp f x =
  let seen = ref IntSet.empty in
  let rec aux f md =
    if IntSet.mem md.i !seen then
      Fmt.string f "..."
    else (
      seen := IntSet.add md.i !seen;
      match md.ty with
      | Constant -> Fmt.string f "(const)"
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
        match md.context with
        | None -> []
        | Some c -> aux c
      in
      let outputs =
        match md.ty with
        | Constant when ctx = [] -> Dot.node f md.i "(const)"; [md]
        | Constant -> ctx
        | Prim name -> Dot.node f md.i name; md :: ctx
        | Bind (x, name) ->
          let inputs = aux x in
          Dot.node f md.i name;
          inputs |> List.iter (fun input -> input ==> md);
          ctx |> List.iter (fun input -> input ==> md);
          [md]
        | Pair (x, y) ->
          aux x @ aux y @ ctx
        | Gate_on { ctrl; value } ->
          let ctrls = aux ctrl in
          let values = aux value in
          Dot.node f md.i "" ~shape:"circle";
          ctrls |> List.iter (fun input -> edge input md ~style:"dashed");
          values |> List.iter (fun input -> input ==> md);
          ctx |> List.iter (fun input -> input ==> md);
          [md]
        | List_map { items; fn } ->
          let items = aux items in
          Dot.node f md.i "map";
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
