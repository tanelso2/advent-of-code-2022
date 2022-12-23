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

let split_on ~sep l =
  let f acc x =
    match acc with
    | curr::rest ->
        if x = sep
        then []::(curr::rest)
        else (x::curr)::rest
    | _ -> failwith "shouldn't happen"
  in
  List.rev @@ List.fold_left f [[]] l

let max_by (l: 'a list) ~key_fn:(f:'a -> int): 'a option =
  let elem = ref None in
  let max_val = ref Int.min_int in
  let iter_fn x =
    let curr_val = f x in
    if curr_val > !max_val
    then
      (max_val := curr_val;
        elem := Some x)
    else ()
  in
  let () = ListLabels.iter l ~f:iter_fn in
  !elem

let max_by_exn l ~key_fn:f =
  match max_by l ~key_fn:f with
  | Some x -> x
  | None -> failwith "max_by_exn failed"

let min_by l ~key_fn:f = max_by l ~key_fn:(fun a -> f a |> Int.neg)

let min_by_exn l ~key_fn:f =
  match min_by l ~key_fn:f with
  | Some x -> x
  | None -> failwith "min_by_exn failed"