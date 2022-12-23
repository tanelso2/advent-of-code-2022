open! Base

module M = struct
  type t = int * int

  let compare (x1,x2) (y1,y2) =
    let r = Int.compare x1 y1 in
    if r = 0
    then Int.compare x2 y2
    else r

  let sexp_of_t =
    [%sexp_of: int * int]
end

include M

include Comparator.Make(M)
