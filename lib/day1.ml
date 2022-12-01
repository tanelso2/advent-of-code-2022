open! Utils 

let parse (s:string) : int list list =
    let f acc x =
        match acc with
        | curr::rest -> 
            let x = String.trim x in
            if x = ""
            then []::(curr::rest)
            else (int_of_string x::curr)::rest
        | _ -> failwith "Shouldn't happen"
    in
    lines_of s
    |> List.fold_left f [[]]
    |> List.filter (fun x -> List.length x > 0)


let part1 (s:string) =
    let elves = parse s in
    elves
    |> List.map (ListLabels.fold_left ~f:(+) ~init:0)
    |> List.sort (fun a b -> Int.neg @@ compare a b)
    |> List.hd

let part2 (s:string) =
    let elves = parse s in
    elves
    |> List.map sum
    |> List.sort (fun a b -> Int.neg @@ compare a b)
    |> Fun.flip Base.List.take 3
    |> sum
