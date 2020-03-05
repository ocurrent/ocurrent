module K1 = Ephemeron.K1
module Server = Cohttp_lwt_unix.Server

module Memprof =
  struct
    type alloc_kind =
      | Minor
      | Major
      | Unmarshalled

    type sample_info = {
        n_samples: int;
        kind: alloc_kind;
        tag: int;
        size: int;
        callstack: Printexc.raw_backtrace;
    }

    type 'a callback = sample_info -> (Obj.t, 'a) Ephemeron.K1.t option

    type 'a ctrl = {
        sampling_rate : float;
        callstack_size : int;
        callback : 'a callback
    }

    let stopped_ctrl = {
        sampling_rate = 0.; callstack_size = 0;
        callback = fun _ -> assert false
    }

    external set_ctrl : 'a ctrl -> unit = "caml_memprof_set"

    let start = set_ctrl
    let stop () = set_ctrl stopped_ctrl
  end

type data = {
  creation_time : float;
  location : string;
  size : int;
}

let rate = ref 0.0
let tracked = ref []

let callback info =
  let open Memprof in
  (* Printf.printf "callback { n_samples = %d; size = %d }\n%!" info.n_samples info.size; *)
  let eph = K1.create () in
  let location =
    match Printexc.backtrace_slots info.callstack with
    | None -> "-"
    | Some slots ->
      slots
      |> Array.to_list
      |> List.filter_map (fun slot ->
          match Printexc.Slot.location slot with
          | None -> None
          | Some { Printexc.filename; line_number; start_char; end_char } ->
            Some (Printf.sprintf "%s:%d (cols %d-%d)" filename line_number start_char end_char)
        )
      |> String.concat "\n"
  in
  K1.set_data eph {
    creation_time = Unix.gettimeofday ();
    location;
    size = info.Memprof.size;
  };
  tracked := eph :: !tracked;
  Some eph

let gc_running = ref false

let rec gc () =
  let open Lwt.Infix in
  Lwt_unix.sleep 10.0 >>= fun () ->
  if !rate = 0.0 then (
    gc_running := false;
    Lwt.return_unit
  ) else (
    tracked := List.filter K1.check_data !tracked;
    gc ()
  )

open Tyxml.Html

let render_row eph =
  match K1.get_data eph with
  | None -> None
  | Some { creation_time; location; size } ->
    Some (tr [
      td [ txt (Query.string_of_timestamp (Unix.gmtime creation_time)) ];
      td [ pre [txt location] ];
      td [ txt (string_of_int size) ];
    ])

let render () =
  Main.template [
    form ~a:[a_action "/heap"; a_method `Post] [
      input ~a:[a_name "csrf"; a_input_type `Hidden; a_value Main.csrf_token] ();
      table [
        tr [
          th [txt "Sample rate (0-1):"];
          td [input ~a:[a_input_type `Number; a_name "rate"; a_value (Printf.sprintf "%f" !rate); a_step None] ()];
          td [input ~a:[a_input_type `Submit; a_name "remove"; a_value "Set"] ()];
        ];
      ]
    ];
    table ~a:[a_class ["table"]]
      ~thead:(thead [
          tr [
            th [txt "Creation time"];
            th [txt "Location"];
            th [txt "Size"];
          ]
        ])
      (List.filter_map render_row !tracked)
  ]

let set_rate new_rate =
  rate := new_rate;
  if new_rate = 0.0 then (
    Memprof.stop ()
  ) else (
    Memprof.(start { sampling_rate = new_rate; callstack_size = 5; callback });
    if not !gc_running then (
      gc_running := true;
      Lwt.async gc
    )
  );
  Server.respond_redirect ~uri:(Uri.of_string "/heap") ()

let handle_post data =
  match List.assoc_opt "rate" data |> Option.value ~default:[] with
  | [""] -> set_rate 0.0
  | [x] ->
    begin match float_of_string_opt x with
      | Some x when x >= 0.0 && x <= 1.0  -> set_rate x
      | _ -> Server.respond_error ~body:"Invalid rate" ()
    end
  | _ -> Server.respond_error ~body:"Bad form submission" ()
