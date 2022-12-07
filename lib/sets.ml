module CharSet = struct
  include Set.Make(Char)

  let of_string s = 
    s 
    |> Base.String.to_list 
    |> of_list
end