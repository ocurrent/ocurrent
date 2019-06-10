open Lwt.Infix

module Input = struct
  class type watch = object
    method pp : Format.formatter -> unit
    method changed : unit Lwt.t
    method release : unit
  end

  type 'a t = unit -> 'a Current_term.Output.t * watch

  let of_fn t = t

  let get (t : 'a t) = t ()

  let pp_watch f t = t#pp f
end

include Current_term.Make(Input)

type 'a term = 'a t

module Var (T : Current_term.S.T) = struct
  type t = {
    mutable current : T.t Current_term.Output.t;
    name : string;
    cond : unit Lwt_condition.t;
  }

  let create ~name current =
    { current; name; cond = Lwt_condition.create () }

  class watch t =
    let v = t.current in
    object
      method pp f = Fmt.string f t.name

      method changed =
        let rec aux () =
          if Current_term.Output.equal T.equal t.current v then
            Lwt_condition.wait t.cond >>= aux
          else
            Lwt.return ()
        in aux ()

      method release = ()
    end

  let get t =
    track (fun () -> t.current, new watch t )

  let set t v =
    t.current <- v;
    Lwt_condition.broadcast t.cond ()

  let update t f =
    t.current <- f t.current;
    Lwt_condition.broadcast t.cond ()
end

let default_trace r inputs =
  Fmt.pr "@[<v2>Evaluation complete:@,\
          Result: %a@,\
          Watching: %a@]@."
    Current_term.(Output.pp Fmt.(unit "()")) r
    Fmt.(Dump.list Input.pp_watch) inputs

module Engine = struct
  let run ?(trace=default_trace) f =
    let rec aux ~old_watches =
      Fmt.pr "Evaluating...@.";
      let r, watches = Executor.run (f ()) in
      List.iter (fun w -> w#release) old_watches;
      trace r watches;
      Fmt.pr "Waiting for inputs to change...@.";
      Lwt.choose (List.map (fun w -> w#changed) watches) >>= fun () ->
      aux ~old_watches:watches
    in
    aux ~old_watches:[]
end
