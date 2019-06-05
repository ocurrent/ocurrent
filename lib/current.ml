module Input = struct
  type t = string
end

include Current_term.Make(Input)
