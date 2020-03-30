module Make (Metadata : sig type t end) = struct
  type 'a t = {
    id : Id.t;
    bind : bind_context;
    ty : metadata_ty;
    v : 'a Dyn.t Current_incr.t;
  }
  and generic = Term : 'a t -> generic
  and bind_context = generic option
  and metadata_ty =
    | Constant of string option
    | Map_input of { source : generic; info : (string, [`Blocked | `Empty_list]) result }
    | Opt_input of { source : generic }
    | State of { source : generic; hidden : bool }
    | Catch of { source : generic; hidden : bool }
    | Map of generic
    | Bind_in of generic * string
    | Bind_out of generic Current_incr.t
    | Primitive of {x : generic; info : string; meta : Metadata.t option Current_incr.t }
    | Pair of generic * generic
    | Gate_on of { ctrl : generic; value : generic }
    | List_map of { items : generic; output : generic Current_incr.t }
    | Option_map of { item : generic; output : generic Current_incr.t }
    | Collapse of { key : string; value : string; input : generic; output : generic }
end
