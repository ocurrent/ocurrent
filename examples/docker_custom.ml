(* Usage: docker_custom.exe

   This fetches the latest nginx image (once a week) and tests it. The test
   starts the container running nginx and then tries execing curl inside it
   too. This demonstrates how to write custom pipeline stages to cover cases
   that the Docker plugin doesn't.
*)

open Current.Syntax

module Docker = Current_docker.Default

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let () = Logging.init ()

module Test = struct
  open Lwt.Infix

  let id = "docker-custom-test"         (* A unique ID for the results database *)

  type t = No_context

  module Key = Docker.Image
  module Value = Current.Unit

  let run image = Docker.Cmd.docker ["container"; "run"; "-d"; Docker.Image.hash image]
  let exec id args = Docker.Cmd.docker ("container" :: "exec" :: "-i" :: id :: args)

  (* The test command to run. You might want to make this part of the key if it
     should be configurable. *)
  let test_command = ["curl"; "-Ss"; "--fail"; "http://localhost/"]

  let build No_context job image =
    Current.Job.start job ~level:Current.Level.Mostly_harmless >>= fun () ->
    (* Start the container running: *)
    Docker.Cmd.with_container ~job ~kill_on_cancel:true (run image) @@ fun id ->
    Current.Job.log job "Waiting 1 second to let HTTP server start...";
    Lwt_unix.sleep 1.0 >>= fun () ->
    (* Test the container's service: *)
    Current.Process.exec ~cancellable:true ~job (exec id test_command)

  let auto_cancel = true

  let pp f image =
    Fmt.pf f "Test %a" Docker.Image.pp image
end

module Test_cache = Current_cache.Make(Test)

(* Test a Docker image by running it and then execing curl inside it. *)
let test image =
  Current.component "test with@,@[<h>%a@]" Fmt.(list ~sep:sp string) Test.test_command |>
  let> image = image in
  Test_cache.get Test.No_context image

(* Build a docker image with nginx and curl and then test it. *)
let pipeline () =
  let dockerfile =
    let+ base = Docker.pull ~schedule:weekly "nginx" in
    `Contents Dockerfile.(
        from (Docker.Image.hash base) @@
        run "apt-get update && apt-get install -y curl --no-install-recommends"
      )
  in
  test (Docker.build ~pull:false ~dockerfile `No_context)

let main config mode =
  let engine = Current.Engine.create ~config pipeline in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode engine;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Check that the nginx container can serve a web page" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner),
  Term.info "docker_custom" ~doc

let () = Term.(exit @@ eval cmd)
