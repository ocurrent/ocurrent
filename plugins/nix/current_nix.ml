module Git = Current_git

open Lwt.Infix
let ( >>!= ) = Lwt_result.bind
open Current.Syntax
module S = Current_cache.S

module Log = struct
  let src = Logs.Src.create "nix" ~doc:"Nix support"
  include (val Logs.src_log src : Logs.LOG)
end

module Drv = struct
  (* TODO reuse same type as Current_cluster.Nix_build.Spec? *)
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

  module Value = Current.String

  let build { pool } job key =
    Current.Job.start job ?pool ~level:Current.Level.Average >>= fun () ->
    Current.Process.check_output ~cancellable:true ~job ("", Key.cmd key |> Array.of_list)
    |> Lwt_result.map String.trim

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

  let fetchgit commit =
    commit |> Current.map (fun commit ->
      "builtins.fetchGit {\n"
      ^ "url = \""^ (Git.Commit_id.repo commit) ^ "\";\n"
      ^ "rev = \""^ (Git.Commit_id.hash commit) ^ "\";\n"
      ^ "}"
    )
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

module type Builder = sig
  type ctx

  val local_pool : ctx -> unit Current.Pool.t option
  
  val eval:
    ?label: string ->
    ctx: ctx ->
    string Current.t ->
    Drv.t Current.t

  val exec:
    ?label: string ->
    ctx: ctx ->
    Exec.t Current.t ->
    string Current.t

  val build:
    ?label: string ->
    ctx: ctx ->
    Drv.t Current.t ->
    string Current.t

end

module Local_build = struct
  type ctx = unit Current.Pool.t option

  let ctx ~pool = pool

  let local_pool ctx = ctx

  let eval ?label ~ctx expr =
    Current.component "nix-instantiate%a" pp_sp_label label |>
    let> expr = expr in
    Raw.eval ?pool:ctx expr

  let build ?label ~ctx drv =
    Current.component "build%a" pp_sp_label label |>
      (* let> _deps_built = Current.list_iter (List.map build_dep deps) in *)
      (* OK I'm really confused by let>, need to investigate this is odd *)
      let> drv = drv in
      Raw.realise_one ?pool:ctx drv

  let exec ?label ~ctx cmd =
    let open Exec in
    Current.component "exec%a" pp_sp_label label |>
      let> _: string = build ~ctx ?label (cmd |> Current.map (fun key -> key.drv))
      and> cmd = cmd
      in Raw.exec ?pool:ctx cmd
end

module Local_build_assert: Builder = struct
  include Local_build
end

module Cluster_build = struct
  module Spec = Cluster_api.Nix_build.Spec
  type ctx = {
    cluster: Current_ocluster.t;
    pool: string;
    local: Local_build.ctx;
  }

  let local_pool ctx = Local_build.local_pool ctx.local

  let ctx ~cluster ~pool ~local = { cluster; pool; local }

  let eval ?label:_ ~ctx:{cluster; pool; _} expr =
    let spec = expr |> Current.map (fun expr ->
      Spec.Eval (`Expr expr)
    ) in
    Current_ocluster.build_nix cluster ~pool spec |> Current.map Drv.unmarshal

  let build ?label:_ ~ctx:{cluster; pool; _} drv =
    let spec = drv |> Current.map (fun Drv.{ drv_path } ->
      Spec.Build (`Drv drv_path)
    ) in
    Current_ocluster.build_nix cluster ~pool spec

  let exec ?label:_ ~ctx:{cluster; pool; _} spec =
    let spec = spec |> Current.map (fun Exec.{ drv = { drv_path }; exe; args } ->
      Spec.(Run {drv = `Drv drv_path; exe; args})
    ) in
    Current_ocluster.build_nix cluster ~pool spec
end

module Cluster_build_assert: Builder = struct
  include Cluster_build
end

module Make (B: Builder) = struct
  module Exec = Exec
  
  let rec _build_deps_then ?label ~ctx drv fn : string Current.t =
    (*
     * NOTE: build_plan currently always runs locally. This should be
     * fine as long as the dependencies are cached (it shouldn't care that it
     * can't actually build derivations for a different platform)
     *)
    let pool = B.local_pool ctx in
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
        let deps_built = Current.return deps
          |> Current.list_iter deps_module
            (fun drv -> build_drv ~ctx drv |> Current.ignore_value)
        in
        fn (Current.gate ~on:deps_built drv)
    )

  and build_drv ?label ~ctx drv : string Current.t =
    _build_deps_then ?label ~ctx drv (B.build ?label ~ctx)

  let eval = B.eval

  let exec = B.exec

  let build_nix ?label ~ctx expr =
    build_drv ?label ~ctx (eval ?label ~ctx expr)

  let fetchgit ?label ?pool commit =
    (* NOTE this is efficient on 1 host (.git cloned once and re-fetched as needed,
     * but wasteful on multi-host (it's only needed on the host that's doing the instantiate)
     * Doing it in parallel on the same host is also bad, because nix doesn't lock concurrent fetches of the same expression properly
     *
     * TODO either restrict to 1 per shared cache dir, or switch to fetchFromGitHub
     *)
    let expr = Eval.fetchgit commit in
    let fetched = (Current.component "fetchgit%a" pp_sp_label label |>
      let> expr = expr in
      Raw.eval ?pool expr
    ) in
    (* We want the side effect of fetching, but don't need the result. There's probably a better way *)
    Current.pair fetched expr |> Current.map snd
end

module Local = struct
  include Make(Local_build)
  let ctx = Local_build.ctx
end

module Remote = struct
  include Make(Cluster_build)
  let ctx = Cluster_build.ctx
end
