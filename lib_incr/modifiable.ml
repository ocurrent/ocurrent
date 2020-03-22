(* Based on "Adaptive Functional Programming"
   https://www.cs.cmu.edu/~guyb/papers/popl02.pdf *)

type changeable = unit

(* Detect attempts to change the inputs in the middle of a propagate operation. *)
let in_propagate = ref false

(* A record of a computation that takes an input of type ['a]. *)
type 'a edge = {
  start : Time.t;               (* When this computation started. *)
  stop : Time.t;                (* When it produced its result. *)
  fn : 'a -> unit;              (* The operation to run on updates. *)
}

(* The state of an initialised modifiable. *)
type 'a full = {
  value : 'a;                   (* The current value. *)
  readers : 'a edge Queue.t     (* The computations which read this value. *)
}

type 'a modval =
  | Uninitialised
  | Full of 'a full

(* A modifiable value starts off [Uninitialised] and then becomes [Full] once the
   initial value is known. When the value changes, it is replaced with a new [Full]
   value. *)
type 'a t = 'a modval ref

module Pq : sig
  (* A priority queue that returns the earliest edge first. *)

  type t

  val create : unit -> t

  val add : t -> unit edge -> unit

  val pop : t -> unit edge option
  (** [pop t] removes and returns the earliest edge in [t]. *)
end = struct
  module Edge_set = Set.Make(struct
      type t = unit edge
      let compare a b = Time.compare a.start b.start
    end)

  type t = Edge_set.t ref

  let create () =
    ref Edge_set.empty

  let add t edge =
    if Time.is_valid edge.start then (
      t := Edge_set.add edge !t;
      Time.set_forget edge.start (fun () -> t := Edge_set.remove edge !t)
    )

  let pop t =
    match Edge_set.min_elt_opt !t with
    | None -> None
    | Some edge ->
      t := Edge_set.remove edge !t;
      Time.clear_forget edge.start;
      Some edge
end

(* The singleton propagation queue. This contains all edges that need to be recalculated. *)
let q = Pq.create ()

let now = ref (Time.root ())

(* Insert a new time directly after [now]. *)
let insert_now ?on_forget () =
  now := Time.after ?on_forget !now;
  !now

let create init =
  let t = ref Uninitialised in
  init t;
  t

let non_empty (t:'a t) =
  match !t with
  | Full x -> x
  | Uninitialised -> failwith "Modifiable is empty! (this shouldn't happen)"

(* If we keep reading a modifiable that doesn't change often, the list of
   readers can build up over time. So each time we add something to the queue,
   we also take one existing item and check that it's still valid. *)
let minor_tidy q =
  match Queue.take_opt q with
  | None -> ()
  | Some edge ->
    if Time.is_valid edge.start then Queue.add edge q

let read t fn =
  let value = (non_empty t).value in
  let start = insert_now () in
  fn value;
  let stop = insert_now () in
  let readers = (non_empty t).readers in         (* Readers might have changed by now *)
  let edge = { start; stop; fn } in
  minor_tidy readers;
  Queue.add edge readers;
  t := Full { value; readers }

let on_release fn =
  let _ : Time.t = insert_now ~on_forget:fn () in
  ()

(* A more efficient version of [read], when we already know the start and stop times. *)
let reread t reader () =
  match !t with
  | Uninitialised -> failwith "Modifiable is empty! (this shouldn't happen)"
  | Full f ->
    minor_tidy f.readers;
    Queue.add reader f.readers;
    reader.fn f.value

let write ~eq t value =
  match !t with
  | Uninitialised -> t := Full { value; readers = Queue.create () }
  | Full { value = old; readers = _ } when eq old value -> ()
  | Full old ->
    t := Full { value; readers = Queue.create () };
    old.readers |> Queue.iter (fun r -> Pq.add q { r with fn = reread t r })

let deref t = (non_empty t).value

let change ~eq t v =
  if !in_propagate then failwith "Current_incr.change called within propagate!";
  let present = !now in
  write ~eq t v;
  now := present

let rec propagate2 () =
  match Pq.pop q with
  | None -> ()
  | Some { start; stop; fn } ->
    (* Note: The later paper splices out after calling [fn] rather than before - why? *)
    Time.splice_out start stop;
    (* They also added a [finger] variable - but never use it. *)
    now := start;
    fn ();
    propagate2 ()

let propagate () =
  assert (not !in_propagate);
  in_propagate := true;
  let ctime = !now in
  propagate2 ();
  now := ctime;
  in_propagate := false
