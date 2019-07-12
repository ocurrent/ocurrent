let state_dir_root = Fpath.v @@ Filename.concat (Sys.getcwd ()) "var"

let state_dir name =
  let name = Fpath.v name in
  assert (Fpath.is_rel name);
  let path = Fpath.append state_dir_root name in
  match Bos.OS.Dir.create path with
  | Ok (_ : bool) -> path
  | Error (`Msg m) -> failwith m
