let state_dir_root = ref None

let state_dir_root ?default () =
  match !state_dir_root with
  | None when default <> None ->
    state_dir_root := default;
    Option.get default
  | None ->
    let module M =
      Directories.Project_dirs (struct
          let qualifier = "org"
          let organization = "ocurrent"
          let application = "current"
        end) in
    let current_root =
      match M.state_dir with
      | None -> failwith "Couldn't determine suitable state directory."
      | Some dir -> Fpath.v dir in
    let name = Unix.realpath Sys.argv.(0) |> Hashtbl.hash |> string_of_int |> Fpath.v in
    let pipeline_root = Fpath.append current_root name in
    state_dir_root := Some pipeline_root;
    pipeline_root
  | Some path -> path

let state_dir name =
  let name = Fpath.v name in
  assert (Fpath.is_rel name);
  let path = Fpath.append (state_dir_root ()) name in
  match Bos.OS.Dir.create path with
  | Ok (_ : bool) -> path
  | Error (`Msg m) -> failwith m
