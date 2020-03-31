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
  | Redirect of (eq:('a -> 'a -> bool) -> 'a -> unit)    (* To write here, just call the function instead. *)

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
  | Redirect _ -> failwith "Got an unexpected Redirect (this shouldn't happen)"

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
  | Redirect _ -> failwith "Modifiable is a redirect! (this shouldn't happen)"
  | Full f ->
    minor_tidy f.readers;
    Queue.add reader f.readers;
    reader.fn f.value

let write ~eq t value =
  match !t with
  | Uninitialised -> t := Full { value; readers = Queue.create () }
  | Redirect f -> f ~eq value
  | Full { value = old; readers = _ } when eq old value -> ()
  | Full old ->
    t := Full { value; readers = Queue.create () };
    old.readers |> Queue.iter (fun r -> Pq.add q { r with fn = reread t r })

module Separate (Map : Map.S) = struct
  (* Normally, if we processed all the elements of a set with a function then
     then we would automatically invalidate all of the work whenever the set changed.
     Instead, we pretend that the read of the set finishes before any of the
     elements are processed, so that changing the set just calls our [update]
     function. Then we manually remove any time periods that are no longer needed
     and create any new ones (for newly added elements). The result of the user
     function is intercepted and turned into an operation to add the result to
     the results map.

     Note that this might cause the output to be written to many times in a
     single propagate, but that shouldn't cause any problems. The first write
     will add all readers to the queue but the final result will be set before
     any of them actually run. *)

  (* The time period of a computation that processed an element of the set.
     There is no [fn] here because an input element cannot change, it can only
     be removed from the set. When an element is removed, times from [start] to
     [stop] (both inclusive) are erased from history. *)
  type period = {
    start : Time.t;               (* When this computation started. *)
    stop : Time.t;                (* When it produced its result. *)
  }

  let map xs_incr (f : Map.key -> 'b t -> changeable) : 'b Map.t t =
    let active : period Map.t ref = ref Map.empty in
    let result = create (fun d -> write ~eq:(==) d Map.empty) in
    let start = insert_now () in        (* When we respond to changes in [xs_incr] and update [active]. *)
    now := Time.after !now;             (* When [result] is written. *)
    let update xs =
      (* Called initially and whenever [xs] changes.
         It runs instantaneously at time [start]. *)
      let saved = !now in
      active := Map.merge (fun key a b ->
          match a, b with
          | None, Some () ->
            (* A new element has been added. Add it to the timeline: *)
            let start = Time.after start in
            now := start;
            (* Run [f key]. When it tries to write the result, add that to [results]: *)
            f key (ref (Redirect (fun ~eq value ->
                let old_map = (non_empty result).value in
                match Map.find_opt key old_map with
                | Some old_value when eq old_value value -> ()
                | _ -> write result (Map.add key value old_map) ~eq:(==);
              )));
            let stop = !now in
            (* Record the time period during which [f key] ran, so we can erase it later. *)
            Some { start; stop }
          | Some _ as existing, Some () ->
            (* An existing element is still present. Keep it. *)
            existing
          | Some old, None ->
            (* An element has been removed. Erase it from history: *)
            Time.splice_out (Time.prev old.start) (Time.next old.stop);
            (* Remove its result from the output: *)
            let old_map = (non_empty result).value in
            write result (Map.remove key old_map) ~eq:(==);
            (* Remove it from [active]: *)
            None
          | None, None -> assert false
        ) !active xs;
      now := saved;
    in
    begin
      let xs = non_empty xs_incr in
      minor_tidy xs.readers;
      update xs.value
      (* Note: [xs] might have been replaced by now. *)
    end;
    (* Arrange to call [update] again if [xs] changes: *)
    let edge = { start; stop = start; fn = update } in
    Queue.add edge (non_empty xs_incr).readers;
    result
end

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
  let ctime = !now in
  in_propagate := true;
  Fun.protect propagate2
    ~finally:(fun () ->
        now := ctime;
        in_propagate := false
      )
