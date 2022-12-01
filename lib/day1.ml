open! Utils 

let parse (s:string) : int list list =
    lines_of s
    |> List.map String.trim
    |> split_on ~sep:""
    |> List.map (List.map int_of_string)
    |> List.filter (fun x -> List.length x > 0)

let rev_compare a b = compare a b |> Int.neg

let rev_compare' = Int.neg <<.. compare

let part1 (s:string) =
    let elves = parse s in
    elves
    |> List.map (ListLabels.fold_left ~f:(+) ~init:0)
    |> List.sort rev_compare'
    |> List.hd

let part2 (s:string) =
    let elves = parse s in
    elves
    |> List.map sum
    |> List.sort rev_compare
    |> Fun.flip Base.List.take 3
    |> sum
