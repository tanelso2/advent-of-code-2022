let lines_of s = 
  String.split_on_char '\n' s 

let trimmed_lines_of s =
  String.split_on_char '\n' s
  |> List.map String.trim


let lines_iteri f s =
  let lines = lines_of s in
  List.iteri (fun line_num l -> 
    String.iteri (fun i c -> f line_num i c) l
  ) lines

let box_trim s =
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
    match (!first_line,!last_line,!furthest_left,!furthest_right) with
    | (Some first, Some last, Some left, Some right) -> (first,last,left,right)
    | _ -> failwith "Couldn't find limits"
  in
  let (first,last,left,right) = find_limits_of_box s in
  let lines = lines_of s in
  let res_lines = ref [] in
  List.iteri (fun line_num line ->
      if line_num >= first && line_num <= last
      then 
        let curr_line = ref [] in
        String.iteri (fun i c -> 
          if i >= left && i <= right
          then curr_line := c::!curr_line
          else ()
        ) line;
        res_lines := (!curr_line |> List.rev |> Base.String.of_char_list)::!res_lines
      else ()
  ) lines;
  !res_lines |> List.rev |> String.concat "\n"