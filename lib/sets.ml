module CharSet = struct
  include Set.Make(Char)

  let of_string s = 
    s 
    |> Base.String.to_list 
    |> of_list
end

module IntPairSet = struct
  include Set.Make(struct
    type t = int * int

    let compare (x1,x2) (y1,y2) =
      let r = Int.compare x1 y1 in
      if r == 0
      then Int.compare x2 y2
      else r
  end)
end