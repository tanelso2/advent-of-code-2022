open Utils

let split_in_half l =
    let open Base in
    let len = List.length l in
    List.split_n l (len / 2)

let parse s =
    trimmed_lines_of s 
    |> List.filter (fun x -> String.length x > 0)

let find_match l1 l2 =
    List.find (fun x -> List.exists ((=) x) l2) l1

let find_match3 l1 l2 l3 =
    List.find (fun x -> List.exists ((=) x) l2 && List.exists ((=) x) l3) l1

let find_match' (l1,l2) = find_match l1 l2

let score c = 
    match c with
    | 'a'..'z' -> Char.code c - Char.code 'a' + 1
    | 'A'..'Z' -> Char.code c - Char.code 'A' + 27
    | _ -> failwith "not a letter"

let find_group_match = function
| [x;y;z] -> 
    let open Base.String in
    find_match3 (to_list x) (to_list y) (to_list z)
| _ -> failwith "A group that isn't size 3 was passed"

let part1 (s:string) =
    let lines = parse s in
    let f s =
        let (a,b) = Base.String.to_list s |> split_in_half in
        (* Stdio.printf "%s\n%s\n" (Base.String.of_char_list a) (Base.String.of_char_list b); *)
        let m = find_match a b in
        (* Stdio.printf "%c\n" m; *)
        score m
    in
    List.map f lines
    |> sum

let part2 (s:string) =
    let lines = parse s in
    let groups = Base.List.chunks_of lines ~length:3 in
    List.map find_group_match groups
    |> List.map score
    |> sum
