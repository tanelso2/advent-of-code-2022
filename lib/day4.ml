open Utils

type range = (int * int)

let parse_range s : range =
    match String.split_on_char '-' s with
    | [x;y] -> (int_of_string x, int_of_string y)
    | _ -> failwith "parse_range failed"

let parse_pair s =
    match String.split_on_char ',' s with
    | [x;y] -> (parse_range x, parse_range y)
    | _ -> failwith "parse_pair failed: line did not have two ranges"

let parse s = 
    trimmed_lines_of s
    |> List.filter (fun x -> String.length x > 0)    
    |> List.map parse_pair

let contains (l1,h1) (l2,h2) =
    (h2 >= h1 && l2 <= l1)

let overlaps (l1,h1) (l2,h2) =
    (l2 <= h1 && l1 <= l2) || (l1 <= h2 && l2 <= l1)


let part1 (s:string) =
    let pairs = parse s in
    List.filter (fun (x,y) -> contains x y || contains y x) pairs
    |> List.length

let part2 (s:string) =
    let pairs = parse s in
    List.filter (fun (x,y) -> overlaps x y) pairs
    |> List.length
