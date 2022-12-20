let sum = (ListLabels.fold_left ~f:(+) ~init:0)

let product = (ListLabels.fold_left ~f:(fun x y -> x * y) ~init:1)

let rec zip2_unequal (l1: 'a list) (l2: 'b list) : ('a * 'b) list = 
  match (l1,l2) with
  | (([],_) | (_,[])) -> []
  | (x::xs, y::ys) -> (x,y)::(zip2_unequal xs ys)

let rec zip3_unequal l1 l2 l3 =
  match (l1,l2,l3) with
  | (x::xs, y::ys, z::zs) -> (x,y,z)::(zip3_unequal xs ys zs)
  | _ -> []

let (@..) low high =
  let rec helper i acc =
    if i > high
    then acc
    else helper (i+1) (i::acc)
  in
  List.rev @@ helper low []