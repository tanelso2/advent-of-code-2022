let lines_of s = 
  String.split_on_char '\n' s 

let sum = (ListLabels.fold_left ~f:(+) ~init:0)

let (<<) f g x = f(g(x))

let (<<..) f g x y = f(g x y)

let rec zip2_unequal (l1: 'a list) (l2: 'b list) : ('a * 'b) list = 
  match (l1,l2) with
  | (([],_) | (_,[])) -> []
  | (x::xs, y::ys) -> (x,y)::(zip2_unequal xs ys)

let rec zip3_unequal l1 l2 l3 =
  match (l1,l2,l3) with
  | (x::xs, y::ys, z::zs) -> (x,y,z)::(zip3_unequal xs ys zs)
  | _ -> []

let split_on ~sep l =
  let f acc x =
    match acc with
    | curr::rest ->
        if x = sep
        then []::(curr::rest)
        else (x::curr)::rest
    | _ -> failwith "shouldn't happen"
  in
  List.fold_left f [[]] l