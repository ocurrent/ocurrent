open Current.Syntax

module S = S

let pp_tag = Fmt.using (Astring.String.cuts ~sep:":") Fmt.(list ~sep:(unit ":@,") string)

module Make (Host : S.HOST) = struct
  module Image = Image

  module Container_result = struct

    type 'a t =
      | Logs : Run.fetch_logs -> string list t
      | File : { path : Fpath.t } -> string t
      | Unit : unit t
      | Pair : 'a t * 'b t -> ('a * 'b) t

    let (&) a b = Pair (a, b)
    let unit = Unit
    let logs ?tail () = Logs { tail }
    let file path = File { path }

    let rec to_yojson : type a. a t -> _ = function
      | Logs { tail } ->
        let tail = match tail with Some t -> `Int t | None -> `String "none" in
        `Assoc [ "logs", `Assoc [ "tail", tail ] ]
      | File { path } ->
        `Assoc [ "file", `Assoc [ "path", `String (Fpath.to_string path) ] ]
      | Unit -> `String "unit"
      | Pair (a, b) -> `List [ to_yojson a; to_yojson b ]

    let simplify t =
      let rec loop : type a. _ -> _ -> a t -> _ = fun logs files -> function
        | Unit -> logs, files
        | Logs l -> Some l, files
        | File { path } -> logs, Fpath.to_string path :: files
        | Pair (a, b) ->
          let logs, files = loop logs files a in
          loop logs files b
      in
      loop None [] t

    let return (logs : string list) (files : (string * string) list) =
      let rec loop : type a. a t -> a = function
        | Logs _ -> logs
        | File { path } -> List.assoc (Fpath.to_string path) files
        | Unit -> ()
        | Pair (a, b) ->
          loop a, loop b
      in
      loop

  end

  module PC = Current_cache.Make(Pull)

  let docker_context = Host.docker_context

  let pull ?label ~schedule tag =
    let label = Option.value label ~default:tag in
    Current.component "pull %s" label |>
    let> () = Current.return () in
    PC.get ~schedule Pull.No_context { Pull.Key.docker_context; tag }

  module BC = Current_cache.Make(Build)

  let pp_sp_label = Fmt.(option (prefix sp string))

  let option_map f = function
    | None -> None
    | Some x -> Some (f x)

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit

  let build ?schedule ?timeout ?(squash=false) ?label ?dockerfile ?pool ~pull src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> dockerfile = Current.option_seq dockerfile in
    let dockerfile = option_map Dockerfile.string_of_t dockerfile in
    BC.get ?schedule { Build.pull; pool; timeout } { Build.Key.commit; dockerfile; docker_context; squash }

  module RC = Current_cache.Make(Run)

  let run ?label ?pool image ~args =
    let fetch_logs = None and fetch_files = [] in
    let+ _ =
      Current.component "run%a" pp_sp_label label |>
      let> image = image in
      RC.get { Run.pool } { Run.Key.image; args; docker_context; fetch_logs; fetch_files }
    in
    ()

  let run' ?label ?pool image ~args ~results =
    let fetch_logs, fetch_files = Container_result.simplify results in
    let+ Run.Value.{ logs; files } =
      Current.component "run%a" pp_sp_label label |>
      let> image = image in
      RC.get { Run.pool } { Run.Key.image; args; docker_context; fetch_logs; fetch_files }
    in
    Container_result.return logs files results

  module TC = Current_cache.Output(Tag)

  let tag ~tag image =
    Current.component "docker-tag@,%a" pp_tag tag |>
    let> image = image in
    TC.set Tag.No_context { Tag.Key.tag; docker_context } { Tag.Value.image }

  module Push_cache = Current_cache.Output(Push)

  let push ?auth ~tag image =
    Current.component "docker-push@,%a" pp_tag tag |>
    let> image = image in
    Push_cache.set auth { Push.Key.tag; docker_context } { Push.Value.image }

  module SC = Current_cache.Output(Service)

  let service ~name ~image () =
    Current.component "docker-service@,%s" name |>
    let> image = image in
    SC.set Service.No_context { Service.Key.name; docker_context } { Service.Value.image }
end

module Default = Make(struct
    let docker_context = Sys.getenv_opt "DOCKER_CONTEXT"
  end)

module MC = Current_cache.Output(Push_manifest)

let push_manifest ?auth ~tag manifests =
  Current.component "docker-push-manifest@,%a" pp_tag tag |>
  let> manifests = Current.list_seq manifests in
  MC.set auth tag { Push_manifest.Value.manifests }
