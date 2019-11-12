open Lwt.Infix

let id = "sandmark"

type t = Current.Pool.t

let sandmark_pre_exec ~bench_core ~arch = 
  Fmt.strf "taskset --cpu-list %S setarch %S --addr-no-randomize" bench_core arch

let outdir = "/tmp/out"

module Config = struct
  type t = {
    run_stages : string;
    executable_spec : string;
    sandmark_tag_override : string option;
    sandmark_comp_fmt : string option;
    upload_project_name : string option;
    environment : string;
    codespeed_url : string;
    bench_core : string;
    arch : string;
  } [@@deriving to_yojson]

  let v 
    ~run_stages
    ~environment
    ~codespeed_url
    ~bench_core
    ~arch
    ~executable_spec
    ?sandmark_tag_override
    ?sandmark_comp_fmt
    ?upload_project_name
    () =
    {
      run_stages;
      executable_spec; sandmark_tag_override; sandmark_comp_fmt; upload_project_name;
      environment; codespeed_url; bench_core; arch;
    }

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Key = struct
  type t = Config.t * Current_git.Commit.t

  let digest (config, commit) =
    Yojson.Safe.to_string @@ `Assoc [
      "config", Config.to_yojson config;
      "commit", `String (Current_git.Commit.id commit);
    ]
end

module Value = Current.Unit

let maybe name = function
  | None -> []
  | Some value -> [name; value]

let cmd _key =
  [ "sleep"; "1h" ]

let build pool job key =
  Current.Job.start job ~pool ~level:Current.Level.Average >>= fun () ->
  Current.Process.exec ~cancellable:true ~job ("", Array.of_list (cmd key))

let auto_cancel = true

let pp f k = Fmt.(list ~sep:sp (quote string)) f (cmd k)
