open Utils

module Tile = struct
    open Base

    type t =
    | Empty
    | Sand
    | Wall
    [@@deriving variants]

    let to_char = function
    | Empty -> '.'
    | Sand -> 'o'
    | Wall -> '#'

end

let draw_wall g (start,finish) =
    Grid.line_from g start finish
    |> List.map fst
    |> List.iter (Grid.set_space g Tile.Wall)

let make_grid_of_lines ls =
    let locs = List.concat_map (fun (a,b) -> [a;b]) ls in
    let max_x = ListUtils.max_by_exn ~key_fn:fst locs |> fst in
    let max_y = ListUtils.max_by_exn ~key_fn:snd locs |> snd in
    let g = Grid.empty_grid (max_x * 2) (max_y + 3) Tile.Empty in
    List.iter (draw_wall g) ls;
    g

let print_grid g =
    Grid.to_string g Tile.to_char

let rec fall_sand g loc =
    let open Base.Continue_or_stop in
    let open Tile in
    let below = Direction.move loc Down in
    match Grid.get_space g below with
    | None -> Stop ()
    | Some Empty -> fall_sand g below
    | Some _ ->
        let below_left = Direction.move below Left in
        match Grid.get_space g below_left with
        | None -> failwith "Shouldn't happen"
        | Some Empty -> fall_sand g below_left
        | Some _ ->
            let below_right = Direction.move below Right in
            match Grid.get_space g below_right with
            | None -> failwith "Shouldn't happen"
            | Some Empty -> fall_sand g below_right
            | Some _ ->
                (* Can't fall anymore, settle here *)
                Grid.set_space g Sand loc;
                Continue ()

let fill_with_sand g =
    let open Base.Continue_or_stop in
     let start_point = (500,0) in
     let g' = Grid.copy g in
     let rec helper g =
        match fall_sand g start_point with
        | Continue () -> 
            if Tile.is_sand (Grid.get_space_exn g start_point)
            then g
            else helper g
        | Stop () -> g
     in
     helper g'

let parse_lines s =
    let line_pairs = ref [] in
    let add_line_pair p =
        line_pairs := (p::!line_pairs)
    in
    let previous = ref None in
    let curr_pair = ref [] in
    let has_buffer () = not @@ Base.List.is_empty !curr_pair in
    let parse_curr_pair () =
        let cs = List.rev !curr_pair in
        curr_pair := [];
        let numbers =
            cs 
            |> ListUtils.split_on ~sep:','
            |> List.map Base.String.of_char_list 
            |> List.map int_of_string
        in
        match numbers with
        | [x;y] -> (x,y)
        | _ -> 
            let str = Base.String.of_char_list cs in
            failwith (Printf.sprintf "error parsing pair \"%s\"" str)
    in
    let f = function
    | '0'..'9' | ',' as d -> curr_pair := (d::!curr_pair)
    | '-' -> 
        let p = parse_curr_pair () in
        (match !previous with
         | None -> previous := Some p
         | Some prev -> 
            add_line_pair (prev, p);
            previous := Some p
        )
    | '\n' -> 
        (match !previous with
         | None -> 
            if has_buffer ()
            then
                failwith "only one pair on a line?"
            else
                (* empty line? *)
                (* just do nothing *)
                ()
         | Some prev ->
            let p = parse_curr_pair () in
            add_line_pair (prev, p);
            previous := None
        )
    | _ -> ()
    in
    String.iter f (String.trim s);
    if has_buffer ()
    then 
        let p = parse_curr_pair () in
        match !previous with
        | None -> failwith "has_buffer but no previous at end"
        | Some prev ->
            add_line_pair (prev, p)
    else ();
    List.rev !line_pairs

let parse = parse_lines >> make_grid_of_lines

let part1 (s:string) =
    let g = parse s in
    let g' = fill_with_sand g in
    Grid.count Tile.is_sand g'

let add_floor g =
    let g = Grid.copy g in
    draw_wall g ((Grid.lower_left g), (Grid.lower_right g));
    g

let part2 (s:string) =
    let g = parse s in
    let g' = add_floor g in
    let g'' = fill_with_sand g' in
    Grid.count Tile.is_sand g''
