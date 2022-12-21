open Utils

type loc = int * int

let above (_,y1) (_,y2) = y1 < y2
let below (_,y1) (_,y2) = y1 > y2

let to_left_of (x1,_) (x2,_) = x1 < x2
let to_right_of (x1,_) (x2,_) = x1 > x2

let are_touching (x1,y1) (x2,y2) =
    let dx = abs (x1 - x2) in
    let dy = abs (y1 - y2) in
    dx <= 1 && dy <= 1


module Rope = struct
    type t = loc list

    let head (v: t) = List.hd v

    let tail (v: t) = Base.List.last_exn v 

    let initial (n:int): t = 
        let init = (0,0) in
        List.init n (fun _ -> init)

    let move_tail (head, tail) =
        if are_touching head tail 
        then tail
        else
        let (tx,ty) = tail in
        let res_y = 
            if above head tail
            then ty - 1
            else if below head tail
            then ty + 1
            else ty
        in
        let res_x =
            if to_left_of head tail
            then tx - 1
            else if to_right_of head tail
            then tx + 1
            else tx
        in
        (res_x,res_y)

    let move dir (rope : t) =
        let prev = ref None in
        let f v =
            let res =
                match !prev with
                | None -> Direction.move v dir
                | Some p -> move_tail (p,v)
            in
            prev := Some res;
            res
        in
        List.map f rope

    let rec do_instructions ?f (rope: t) ins = 
        begin
        match f with
        | None -> ()
        | Some fn -> fn rope
        end;
        match ins with
        | (d,n)::rest -> 
            let rope' = move d rope in
            let ins' = if n > 1
                       then ((d,n-1)::rest)
                       else rest 
            in
            do_instructions ?f rope' ins'
        | [] -> ()
end

let parse_line l =
    match String.split_on_char ' ' l with
    | [d;s] -> (Direction.of_string d, int_of_string s)
    | _ -> failwith "parse_line failed"

let parse s =
    s 
    |> trimmed_lines_of
    |> List.filter (fun l -> String.length l > 0)
    |> List.map parse_line

let find_tail_visited_spaces n ins =
    let open Sets in
    let visited_spaces = ref IntPairSet.empty in
    let visit_space r =
        let tl = Rope.tail r in
        visited_spaces := IntPairSet.add tl !visited_spaces
    in
    Rope.do_instructions ~f:visit_space (Rope.initial n) ins;
    IntPairSet.cardinal !visited_spaces

let part1 (s:string) =
    parse s |> find_tail_visited_spaces 2

let part2 (s:string) =
    parse s |> find_tail_visited_spaces 10
