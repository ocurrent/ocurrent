module Key = struct
  type t = < >
  let compare = compare
end

type t = Key.t
let mint () = object end
let equal = (=)

module Set = Set.Make(Key)
module Map = Map.Make(Key)
