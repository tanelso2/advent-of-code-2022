open Geom.Position
open Utils


module Block = struct
    type blockType = Row | Cross | BackL | Column | Square

    (* To keep things clear, the x,y position of a block will be the lower left of the block *)
    type t = {
        kind: blockType;
        pos: pos;
    }

    let block_order = [| Row; Cross; BackL; Column; Square |]
    let make_block_buffer () = ref @@ RingBuffer.of_array block_order

    let underneath (x,y) = function
    | Row -> [0;1;2;3] |> List.map (fun dx -> (x + dx, y - 1))
    | Cross -> [(x,y); (x+1, y-1); (x+2, y)]
    | BackL -> [0;1;2] |> List.map (fun dx -> (x + dx, y - 1))
    | Column -> [(x, y - 1)]
    | Square -> [0;1] |> List.map (fun dx -> (x + dx, y - 1))

    let under {kind; pos} = underneath pos kind

    let any_under g (b: t) =
        let {pos; _} = b in
        let (_,y) = pos in
        if y = 0 then true
        else
        under b |> List.exists (fun pos -> match Grid.get_space g pos with
                                           | None -> false
                                           | Some v -> v)
    let to_right (x,y) = function
    | Row -> [(x + 4, y)]
    | Cross -> [(x+2,y); (x+3, y+1); (x+2, y+2)]
    | BackL -> Base.List.range ~stop:`inclusive 0 2 |> List.map (fun dy -> (x + 3, y + dy))
    | Column -> Base.List.range ~stop:`inclusive 0 3 |> List.map (fun dy -> (x + 1, y + dy))
    | Square -> [(x+2, y); (x+2, y + 1)]

    let to_left (x,y) = function
    | Row -> [(x - 1, y)]
    | Cross -> [(x,y); (x-1, y+1); (x, y+2)]
    | BackL -> [(x-1, y); (x+1, y+1); (x+1, y+2)]
    | Column -> Base.List.range ~stop:`inclusive 0 3 |> List.map (fun dy -> (x - 1, y + dy))
    | Square -> [(x-1, y); (x-1, y + 1)]

    let move_left (x,y) _ =
        if x = 0
        then (x,y)
        else (x - 1, y)

    let left g {pos; kind} =
        if to_left pos kind |> List.exists (fun pos -> match Grid.get_space g pos with
                                                       | None -> false
                                                       | Some v -> v)
        then
            {pos;kind}
        else 
            let pos' = move_left pos kind in
            {pos = pos'; kind = kind}


    (* How far away is the right edge for a given block *)
    let right_edge = function
    | Row -> 3
    | Cross -> 2
    | BackL -> 3
    | Column -> 0
    | Square -> 1

    let move_right (x,y) b =
        let r = x + right_edge b in
        if r >= 6
        then (x,y)
        else (x + 1, y)

    let right g {pos; kind} =
        if to_right pos kind |> List.exists (fun pos -> match Grid.get_space g pos with
                                                       | None -> false
                                                       | Some v -> v)
        then
            {pos;kind}
        else 
            let pos' = move_right pos kind in
            {pos = pos'; kind = kind}

    let squares {kind; pos} =
        let (x,y) = pos in
        match kind with
        | Row -> Base.List.range ~stop:`inclusive 0 3 |> List.map (fun dx -> (x + dx, y))
        | Cross -> [(x, y+1); (x+1, y); (x+1, y+1); (x+1, y+2); (x+2, y+1)]
        | BackL -> [(x,y); (x+1, y); (x+2, y); (x+2, y+1); (x+2, y+2)]
        | Column -> Base.List.range ~stop:`inclusive 0 3 |> List.map (fun dy -> (x, y + dy))
        | Square -> [(x,y); (x+1, y); (x, y+1); (x+1, y+1)]
end

module Cave = struct
    type t = {
        mutable grid: bool Grid.t;
        mutable highest_placed: int;
    }

    let create () : t =
        let grid = Grid.empty_grid 7 8 false in
        let highest_placed = -1 in
        {grid; highest_placed}

    let mark_squares (g: t) b = 
        let squares = Block.squares b in
        let mark (x,y) =
            let h = Grid.height g.grid in
            if y >= h
            then g.grid <- Grid.increase_height g.grid h false
            else ();
            if y > g.highest_placed
            then begin
                (* Stdio.printf "Increasing highest_placed to %d\n" y; *)
                g.highest_placed <- y
            end
            else ();
            Grid.set_space g.grid true (x,y)
        in
        List.iter mark squares;
        g

    let next_start (g: t) =
        (2, g.highest_placed + 4)

    let print (g: t) = 
        Grid.to_string g.grid (function | true -> '#' | false -> '.')
        |> Stdio.printf "%s\n"

end

module AirGust = struct
    type t = Left | Right
    let parse_air_gust = function
    | '<' -> Some Left
    | '>' -> Some Right
    | _ -> None
end

let rec handle_block (c: Cave.t) (b: Block.t) (gusts) : Cave.t =
    let open Block in
    (* let {pos; _} = b in *)
    (* Stdio.printf "%d,%d\n" (fst pos) (snd pos); *)
    let gust = RingBuffer.get_val gusts in
    let open AirGust in
    let b' = match gust with
             | Left -> Block.left c.grid b
             | Right -> Block.right c.grid b
    in
    if Block.any_under (c.grid) b'
    then begin
        (* let (x,y) = b'.pos in *)
        (* Stdio.printf "Coming to rest at %d,%d\n" x y; *)
        Cave.mark_squares c b'
    end
    else 
       let (x',y') = b'.pos in
       let y'' = y' - 1 in
       let b'' = {b' with pos = (x', y'')} in
       handle_block c b'' gusts
        

let parse s = 
    String.to_seq s 
    |> Seq.filter_map AirGust.parse_air_gust 
    |> Array.of_seq
    |> RingBuffer.of_array
    |> ref


let part1 (s:string) =
    let wind_gusts = parse s in
    let blocks = Block.make_block_buffer () in
    let cave = Cave.create () in
    Base.List.range 0 2022 |> List.iter (fun _ ->
        let starting_pos = Cave.next_start cave in
        let kind = RingBuffer.get_val blocks in
        let open Block in
        let b = {pos=starting_pos; kind=kind} in
        let _ = handle_block cave b wind_gusts in
        (* Cave.print c;
        Stdio.printf "\n\n"; *)
        ()
    );
    cave.highest_placed

let part2 (s:string) =
    let _ = s in
    failwith "NOIMPL"
