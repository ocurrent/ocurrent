open Lwt.Infix

type t = No_context

let ( >>!= ) = Lwt_result.bind

module Key = struct
  type t = {
    fqdn : string;
  } [@@deriving to_yojson]

  let pp f t = Yojson.Safe.pretty_print f (to_yojson t)

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    fqdn : string;
    result : string;
  } [@@deriving yojson]
    
  let digest t = Yojson.Safe.to_string (to_yojson t)

  let marshal t = to_yojson t |> Yojson.Safe.to_string
  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e
end

let id = "dig"

let build No_context job { Key.fqdn } =
  Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
  let cmd = ("dig", [| "dig"; "@8.8.8.8"; "+noall"; "+answer"; fqdn; "A"; fqdn; "AAAA" |]) in
  Current.Process.check_output ~cancellable:false ~job cmd >|= Stdlib.Result.map @@ fun result ->
  Current.Job.log job "Returned %s" result;
  { Value.fqdn; result }

let pp f key = Fmt.pf f "dig %a" Key.pp key

let auto_cancel = false
