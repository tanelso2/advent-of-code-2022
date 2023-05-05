open! Base

module M = struct
  type t = string * string
  let compare (x1,x2) (y1,y2) =
    let r = String.compare x1 y1 in
    if r = 0
    then String.compare x2 y2
    else r

  let sexp_of_t =
    [%sexp_of: string * string]
end

include M

include Comparator.Make(M)