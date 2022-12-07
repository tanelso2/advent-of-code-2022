let lines_of s = 
  String.split_on_char '\n' s 

let trimmed_lines_of s =
  String.split_on_char '\n' s
  |> List.map String.trim

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

let lines_iteri f s =
  let lines = lines_of s in
  List.iteri (fun line_num l -> 
    String.iteri (fun i c -> f line_num i c) l)
    lines

let find_limits_of_box s =
  let first_line = ref None in
  let last_line = ref None in
  let furthest_left = ref None in
  let furthest_right = ref None in
  let f line_num i c =
      if c != ' '
      then 
        match !first_line with
        | None -> first_line := Some line_num
        | _ -> ()
        ;
        last_line := Some line_num
        ;
        match !furthest_left with
        | None -> furthest_left := Some i
        | Some curr_left -> 
            if i < curr_left
            then furthest_left := Some i
            else ()
        ;
        match !furthest_right with
        | None -> furthest_right := Some i
        | Some curr_right -> 
            if i > curr_right
            then furthest_right := Some i
          else ()
      else ()
  in
  lines_iteri f s;
  match (!furthest_left,!furthest_right,!first_line,!last_line) with
  | (Some left, Some right, Some first, Some last) -> (first,last,left,right)
  | _ -> failwith "Couldn't find limits"

let box_trim s =
  let (first,last,left,right) = find_limits_of_box s in
  let lines = lines_of s in
  let res_lines = ref [] in
  List.iteri (fun line_num line ->
      if line_num >= first && line_num <= last
      then 
        let curr_line = ref [] in
        String.iteri (fun i c -> 
          if line_num >= first && line_num <= last && i >= left && i <= right
          then curr_line := c::!curr_line
          else ()
        ) line;
        res_lines := (!curr_line |> List.rev |> Base.String.of_char_list)::!res_lines
      else ()
  ) lines;
  !res_lines |> List.rev |> String.concat "\n"

module Stack = struct
  include Stack
  let pop_n s i = 
      let rec helper s i acc =
          if i <= 0
          then acc
          else 
              let x = Stack.pop s in
              helper s (i - 1) (x::acc)
      in
      helper s i []
end