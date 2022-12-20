open Utils.ListUtils

let parse s =
    Grid.parse s (fun c -> Base.String.of_char c |> int_of_string)

let find_seeable_in_line l =
    let max_height = ref Int.min_int in
    let seen = ref [] in
    List.iter (fun (loc, h) ->
        if h > !max_height
        then begin
            max_height := h;
            seen := (loc::!seen)
        end
        else ()
        ) l;
    !seen

let find_seeable_trees g =
    let from_left = Grid.rowsi g in
    let from_right = List.map List.rev from_left in
    let from_up = Grid.columnsi g in
    let from_down = List.map List.rev from_up in
    let seeable = List.concat_map 
        (fun ls -> List.concat_map find_seeable_in_line ls) 
        [from_left;from_right;from_up;from_down] 
    in
    Sets.IntPairSet.of_list seeable

let find_seeable_trees_from_height g loc h =
    let open Base.Continue_or_stop in
    let open Grid in
    let dirs = [above;below;to_left_of;to_right_of] in
    let find_in_dir d =
        Base.List.fold_until ~init:0 ~finish:Fun.id
        ~f:(fun acc (_,v) -> if v >= h
                             then Stop (acc + 1) 
                             else Continue (acc + 1)
            )
        (d g loc)
    in
    List.map find_in_dir dirs

let find_scenic_score g loc =
    let h = Grid.get_space_exn g loc in
    find_seeable_trees_from_height g loc h
    |> product

let find_best_scenic_score g =
    let best_score = ref Int.min_int in
    let f loc _ =
        let open Utils.RefUtils in
        let score = find_scenic_score g loc in
        replace_max best_score score
    in
    Grid.iteri f g;
    !best_score

let part1 (s:string) =
    parse s
    |> find_seeable_trees
    |> Sets.IntPairSet.cardinal

let part2 (s:string) =
    parse s |> find_best_scenic_score
