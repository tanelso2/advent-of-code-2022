open Maps
open Utils

let rec parse_height_and_endpoints sp ep loc = function
| 'S' -> begin 
    sp := Some loc;
    parse_height_and_endpoints sp ep loc 'a'
end
| 'E' -> begin
    ep := Some loc;
    parse_height_and_endpoints sp ep loc 'z'
end
| 'a'..'z' as c -> Char.code c - Char.code 'a'
| c -> failwith (Printf.sprintf "Unknown character %c\n" c)

let parse s = 
    let s = box_trim s in
    let startpoint = ref None in
    let endpoint = ref None in
    let g = Grid.parsei s (parse_height_and_endpoints startpoint endpoint) in
    match (!startpoint, !endpoint) with
    | (Some sp, Some ep) -> (sp, ep, g)
    | _ -> failwith "Could not find both startpoint and endpoint"

module MinHeap = struct
    type t = (int IntPairMap.t) ref

    let empty () : t = ref @@ IntPairMap.empty ()

    let pop (h: t) =
        let open Base.Option.Let_syntax in
        let%map (k,v) = Map.min_by !h ~key_fn:(fun _ v -> v) in
        let h' = Map.remove !h k in
        h := h';
        (k, v)

    let lower_val (h: t) k v =
        let f = function
        | None -> Some v
        | Some v' -> Some (min v v')
        in
        h := Map.change ~f:f !h k 

    let is_empty (h: t) =
        Map.is_empty !h
end

let reachable curr dest =
    dest - curr <= 1

let find_shortest (g: int Grid.t) sp ep : (int, string) Result.t =
    let open Sets in
    let module H = MinHeap in
    let heap = H.empty () in
    let visited = ref IntPairSet.empty in
    let rec helper () =
        match H.pop heap with
        | None -> 
            let msg = Printf.sprintf "No more to check, route not found\nVisited: %d/%d\n" (IntPairSet.cardinal !visited) (Grid.height g * Grid.width g) in
            Result.Error msg
        | Some (loc, v) -> 
            if ep = loc
            then Result.Ok v
            else if IntPairSet.mem loc !visited
            then helper ()
            else
            let height = Grid.get_space_exn g loc in
            let candidates = 
                Grid.get_cardinal_neighbors g loc
                |> List.filter (fun (loc, h) -> reachable height h && not (IntPairSet.mem loc !visited))
            in
            visited := IntPairSet.add loc !visited;
            List.iter (fun (loc,_) -> H.lower_val heap loc (v + 1)) candidates;
            helper ()
    in
    H.lower_val heap sp 0;
    helper ()

let part1 (s:string) =
    let (sp, ep, g) = parse s in
    Result.get_ok @@ find_shortest g sp ep

let part2 (s:string) =
    let (_, ep, g) = parse s in
    let candidates = Grid.collect ((=) 0) g in
    let find_shortest' sp =
        Result.to_option @@ find_shortest g sp ep
    in
    candidates
    |> List.map snd
    |> List.filter_map find_shortest'
    |> Base.List.reduce_exn ~f:min
