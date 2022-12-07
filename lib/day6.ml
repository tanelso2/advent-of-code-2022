module CharSet = Set.Make(Char)

let find_first_n_different n s =
    let f i _ =
        if i > n
        then 
            let sub = String.sub s (i - n) n in
            sub
            |> Base.String.to_list
            |> CharSet.of_list
            |> CharSet.cardinal 
            |> ((=) n)
        else false
    in
    match Base.String.findi s ~f:f with
    | None -> failwith "different not found"
    | Some (i,_) -> i

let find_first_four_different s =
    find_first_n_different 4 s

let part1 (s:string) =
    find_first_four_different (String.trim s)

let part2 (s:string) =
    find_first_n_different 14 (String.trim s)
