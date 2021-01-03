module Git = Current_git
module Github = Current_github

open Lwt.Infix
let ( >>!= ) = Lwt_result.bind
open Current.Syntax
module S = Current_cache.S

module Log = struct
  let src = Logs.Src.create "nix" ~doc:"Nix support"
  include (val Logs.src_log src : Logs.LOG)
end

module Drv = struct
  type t = {
    drv_path : string;
  }

  let cmd { drv_path } =
    [ "nix-store"; "--realise"; drv_path ]

  (* let pp_cmd = Fmt.(list ~sep:sp (quote string)) *)
  let pp f { drv_path } = (Fmt.quote Fmt.string) f drv_path

  let pp_short f { drv_path } = Fmt.string f
    (String.index_opt drv_path '-' |> Option.fold ~none:drv_path ~some:(fun i ->
      String.sub drv_path (i+1) (String.length drv_path - (i + 1))
    ))

  let digest { drv_path } =
    Yojson.Safe.to_string @@ `Assoc [
      "drv", `String drv_path;
    ]
    
  let marshal { drv_path } = drv_path

  let unmarshal drv_path = { drv_path }
  
  let equal { drv_path = drva } { drv_path = drvb } = String.equal drva drvb

  let compare { drv_path = drva } { drv_path = drvb } = String.compare drva drvb
end

module Drv_list = struct
  type t = Drv.t list

  let pp f = Fmt.pf f "[%a]" (Fmt.list Drv.pp)

  let digest drvs = String.concat "\n" (List.map Drv.digest drvs)

  (* TODO this could be cleaner as JSON *)
  let marshal drvs = String.concat "\n" (List.map Drv.marshal drvs)

  let unmarshal = function
    | "" -> []
    | str -> String.split_on_char '\n' str |> List.map Drv.unmarshal
end

module Build = struct
  let id = "nix-store-build"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = Drv

  module Value = Current.Unit

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    Current.Process.exec ~cancellable:true ~job ("", Key.cmd key |> Array.of_list)

  let pp = Drv.pp

  let auto_cancel = true
end

module Build_plan = struct
  (*
  We use a combination of:

  nix-store --realise --dry-run: get the set of possible inputs that need building
  nix-store --query --references: get the set of direct dependencies

  Many dependencies don't need to be built, and --dry-run prints transitive deps.
  So we take the intersection to get "direct deps that actually need building"
  *)
  let id = "nix-plan"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = Drv

  (* TODO indicate whether self needs build? *)
  module Value = Drv_list
  
  let copy_to_log ~job src =
    let rec aux () =
      Lwt_io.read ~count:4096 src >>= function
      | "" -> Lwt.return_unit
      | data -> Current.Job.write job data; aux ()
    in
    aux ()
    
  module StringSet = Set.Make(String)

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let references_cmd = [ "nix-store"; "--query"; "--references"; key.Key.drv_path ] in
    let dry_run_cmd = [ "nix-store"; "--realise"; "--dry-run"; key.Key.drv_path ] in
    
    let lines ~desc channel =
      Lwt_io.read_lines channel
      |> Lwt_stream.map (fun line ->
        Current.Job.log job "[%s] %s" desc line;
        String.trim line
      )
      |> Lwt_stream.filter (fun line ->
        let result = not (String.equal line "") in
        Log.info (fun f -> f "filtering line -> %a // %a"
          (Fmt.quote Fmt.string) line
          Fmt.bool result
        );
        not (String.equal line ""))
      |> Lwt_stream.to_list
      |> Lwt.map (fun lines ->
        Log.info (fun f -> f "filtered stream -> [%a]" (Fmt.list (Fmt.quote Fmt.string)) lines);
        lines
      )
    in

    (* TODO could run in parallel *)
    Current.Process.exec_with ~cancellable:true ~job ("", dry_run_cmd |> Array.of_list) (fun proc ->
      (* can't use check_output since dry-run outputs to stderr *)
      let copy_thread = copy_to_log ~job proc#stdout in
      let result = lines ~desc:"dry-run" proc#stderr |> Lwt.map Result.ok in
      copy_thread >>= fun () -> result
    ) >>= fun dry_run ->
    Current.Process.exec_with ~cancellable:true ~job ("", references_cmd |> Array.of_list) (fun proc ->
      let copy_thread = copy_to_log ~job proc#stderr in
      let result = lines ~desc:"references" proc#stdout |> Lwt.map Result.ok in
      copy_thread >>= fun () -> result
    ) >>= fun references ->
    (* TODO ugly *)
    Lwt.return (Result.bind references (fun references ->
      dry_run |> Result.map (fun dry_run ->
        let intersection = StringSet.inter
          (StringSet.of_list references)
          (StringSet.of_list dry_run)
          |> StringSet.elements
          |> List.sort String.compare
        in

        let result = intersection |> List.map Drv.unmarshal in
        Current.Job.log job "Found required dependencies: [%a]" Drv_list.pp result;
        result
      )
    ))

  let pp = Key.pp

  let auto_cancel = true
end

(* evaluate a .nix expression into a concrete .drv file *)
module Eval = struct
  let id = "nix-eval"

  type t = {
    pool : unit Current.Pool.t option;
  }

  (* TODO this should track nixpkgs & opam2nix hashes *)
  module Key = Current.String
  module Value = Build.Key

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = [ "nix-instantiate"; "--expr"; key ] |> Array.of_list in
    Current.Process.check_output ~cancellable:true ~job ("", cmd) >>!= fun drv ->
    let drv = String.trim drv in
    Current.Job.log job "Resolved -> %S" drv;
    Lwt_result.return { Build.Key.drv_path = drv }

  let pp = Key.pp

  let auto_cancel = true
end

module Exec = struct
  type t = {
    drv : Drv.t;
    exe: string;
    args: string list;
  }

  let pp f { drv; exe; args } =
    Fmt.pf f "drv=%a, exe=%a, args=%a"
      Drv.pp drv
      Fmt.string exe
      (Fmt.list Fmt.string) args

  let digest { drv; exe; args } =
    let Drv.{drv_path} = drv in
    Yojson.Safe.to_string @@ `Assoc [
      "drv", `String drv_path;
      "exe", `String exe;
      "args", `List (args |> List.map (fun a -> `String a));
    ]
end

module Exec_impl = struct
  let id = "nix-exec"

  type t = {
    pool : unit Current.Pool.t option;
  }

  module Key = Exec

  module Value = Current.String

  open Exec

  let build { pool } job { drv; exe; args } =
    (* drv should have already been built, but we have to run instantiate to
     * ensure its result is present locally *)
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    let cmd = Drv.cmd drv in
    Current.Process.check_output ~cancellable:true ~job ("", cmd |> Array.of_list) >>!= fun impl ->
    let cmd = [Filename.concat impl exe] @ args in
    Current.Process.check_output ~cancellable:true ~job ("", cmd |> Array.of_list)

  let pp = Key.pp

  let auto_cancel = true
end

module Raw = struct
  module RealiseC = Current_cache.Make(Build)

  let realise_one ?pool drv =
    RealiseC.get { Build.pool } drv
    
  module Build_planC = Current_cache.Make(Build_plan)
  
  (* TODO rename deps *)
  let build_plan ?pool drv =
    Build_planC.get { Build_plan.pool } drv

  module EvalC = Current_cache.Make(Eval)

  let eval ?pool expr =
    EvalC.get { Eval.pool } expr

  module ExecC = Current_cache.Make(Exec_impl)

  let exec ?pool cmd =
    ExecC.get { Exec_impl.pool } cmd
end

let pp_sp_label = Fmt.(option (prefix sp string))

module Nix = struct
  let rec _build_deps_then ?label ?pool drv fn =
    let deps = Current.component "plan%a" pp_sp_label label |>
      let> drv = drv in
      Raw.build_plan ?pool drv
    in

    let deps_module = (module struct
      type t = Drv.t
      let compare = Drv.compare
      let pp = Drv.pp_short
    end : Current_term.S.ORDERED with type t = Drv.t) in

    deps |> Current.bind ~info:(Current.component "deps%a" pp_sp_label label) (function
      | [] -> fn drv
      | deps ->
        Log.info (fun f -> f "bind deps -> %a" Drv_list.pp deps);
        let deps_built = Current.return deps |> Current.list_iter deps_module (build_drv ?pool) in
        fn (Current.gate ~on:deps_built drv)
    )

  and build_drv ?label ?pool drv =
    _build_deps_then ?label ?pool drv (fun drv ->
      Current.component "build%a" pp_sp_label label |>
        (* let> _deps_built = Current.list_iter (List.map build_dep deps) in *)
        (* OK I'm really confused by let>, need to investigate this is odd *)
        let> drv = drv in
        Raw.realise_one ?pool drv
    )

  (* let rec shell ?label ?pool drv = *)
  (*   _build_deps_then ?label ?pool drv (fun drv -> *)
  (*     Current.component "build%a" pp_sp_label label |> *)
  (*       (* let> _deps_built = Current.list_iter (List.map build_dep deps) in *) *)
  (*       (* OK I'm really confused by let>, need to investigate this is odd *) *)
  (*       let> drv = drv in *)
  (*       Raw.realise_one ?pool drv *)
  (*   ) *)
  let exec ?label ?pool cmd =
    let open Exec in
    Current.component "exec%a" pp_sp_label label |>
      let> () = build_drv ?pool ?label (cmd |> Current.map (fun key -> key.drv))
      and> cmd = cmd
      in Raw.exec ?pool cmd

  let eval ?label ?pool expr =
    Current.component "nix-instantiate%a" pp_sp_label label |>
    let> expr = expr in
    Raw.eval ?pool expr

  let build_nix ?label ?pool expr =
    build_drv ?label ?pool (eval ?label ?pool expr)

  let fetchgit ?label ?pool commit =
    (* NOTE this is efficient on 1 host (.git cloned once and re-fetched as needed,
     * but wasteful on multi-host (it's only needed on the host that's doing the instantiate)
     * Doing it in parallel on the same host is also bad, because nix doesn't lock concurrent fetches of the same expression properly
     *)
    let expr = commit |> Current.map (fun commit ->
      "builtins.fetchGit {\n"
      ^ "url = \""^ (Git.Commit_id.repo commit) ^ "\";\n"
      ^ "rev = \""^ (Git.Commit_id.hash commit) ^ "\";\n"
      ^ "}"
    ) in

    let fetched = (Current.component "fetchgit%a" pp_sp_label label |>
      let> expr = expr in
      Raw.eval ?pool expr
    ) in
    (* We want the side effect of fetching, but don't need the result. There's probably a better way *)
    Current.pair fetched expr |> Current.map snd
end

include Nix (* public API *)
