open Utils

let parse s =
    String.trim s 
    |> lines_of 
    |> List.map String.trim 
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string 

let count_largers (vals : int list) =
    let prev = ref None in
    let count = ref 0 in
    let f v =
        (match !prev with
         | None -> ()
         | Some x -> if v > x
                     then incr count
                     else ()
        );
        prev := Some v
    in
    List.iter f vals;
    !count

let windows_of_3 vs =
    let open List in
    zip3_unequal vs (tl vs) (tl (tl vs))

let part1 (s:string) =
    let _ = s in
    let vals = parse s in
    count_largers vals

let part2 (s:string) =
    let _ = s in
    let vals = parse s in
    vals 
    |> windows_of_3 
    |> List.map (fun (x,y,z) -> x+y+z) 
    |> count_largers
