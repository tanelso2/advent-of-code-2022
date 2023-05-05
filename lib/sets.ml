module CharSet = struct
  include Set.Make(Char)

  let of_string s = 
    s 
    |> Base.String.to_list 
    |> of_list
end

module IntPairSet = struct
  include Set.Make(Utils.IntPair)
end

module StringSet = struct
  include Set.Make(String)
end