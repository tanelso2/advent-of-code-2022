open Utils
open Geom
open Position

let manhattan_distance (x1,y1) (x2,y2) =
    abs (x1 - x2) + abs (y1 - y2)

module Sensor = struct
    type t = {
        sensor: int * int;
        beacon: int * int;
    }
    
    let radius {sensor;beacon;_} =
        manhattan_distance sensor beacon
    
    let left_bound t =
        let (x,_) = t.sensor in
        x - radius t

    let right_bound t =
        let (x,_) = t.sensor in
        x + radius t

    let re = Re.Posix.compile_pat {|Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)|}

    let parse s =
        let res = Re.exec re s in
        let get i = Re.Group.get res i |> int_of_string in
        {
            sensor = (get 1, get 2);
            beacon = (get 3, get 4);
        }

    let covers t x =
        radius t >= manhattan_distance t.sensor x

    let coverage_range t =
        let r = radius t in
        let {sensor;_} = t in
        let (x,y) = sensor in
        let open Sets in
        let res = ref IntPairSet.empty in
        let iter_radius curr_r =
            List.iter (fun i -> 
                let dx = i in
                let dy = curr_r - i in
                res := IntPairSet.add_seq (List.to_seq
                                            [
                                                (x+dx,y+dy);
                                                (x-dx,y+dy);
                                                (x+dx,y-dy);
                                                (x-dx,y-dy);
                                            ])
                                           !res
                ) (0 @.. curr_r)
        in
        List.iter iter_radius (0 @.. r);
        !res

    type corners = {
        ylb: pos;
        yub: pos;
        xub: pos;
        xlb: pos;
    }

    let corners t =
        let (cx,cy) = t.sensor in
        let r = radius t in
        let ylb = (cx, cy - r) in
        let yub = (cx, cy + r) in
        let xlb = (cx - r, cy) in
        let xub = (cx + r, cy) in
        {ylb;yub;xlb;xub}

    type edges = {
        nw: Segment.t; 
        ne: Segment.t;
        sw: Segment.t;
        se: Segment.t;
    }

    let edges t = 
        let {ylb;yub;xlb;xub} = corners t in
        let nw = Segment.from_points xlb ylb in
        let ne = Segment.from_points ylb xub in
        let se = Segment.from_points yub xub in
        let sw = Segment.from_points xlb yub in
        {nw;ne;se;sw}

    let candidate_segments t = 
        let {ylb;yub;xlb;xub} = corners t in
        let mod_x f (x,y) = (f x, y) in
        let mod_y f (x,y) = (x, f y) in
        let plus_1 = succ in
        let minus_1 = pred in
        let ylb' = mod_y minus_1 ylb in
        let yub' = mod_y plus_1 yub in
        let xub' = mod_x plus_1 xub in
        let xlb' = mod_x minus_1 xlb in
        let open Segment in
        [
            from_points xlb' ylb';
            from_points ylb' xub';
            from_points yub' xub';
            from_points xlb' yub';
        ]

    let occlude t (s: Segment.t) =
        match s with
        | Point (px,py) -> 
            if covers t (px,py) 
            then []
            else [s]
        | Segment _ ->
        let {nw;ne;se;sw} = edges t in
        let (b1,b2) =
            match Segment.slope s with
            | 1 -> (nw, se)
            | -1 -> (ne,sw)
            | _ -> failwith "day15 sensors should only be operating on lines with slopes of -1 or 1"
        in
        Segment.occlude_segment s b1 b2

end

let covered_by sensors loc =
    ListUtils.some ~f:(fun s -> Sensor.covers s loc) sensors

let has_beacon sensors loc =
    let open Sensor in
    ListUtils.some ~f:(fun {beacon;_} -> beacon = loc) sensors

let has_sensor sensors loc =
    let open Sensor in
    ListUtils.some ~f:(fun {sensor;_} -> sensor = loc) sensors

let is_invalid sensors loc =
    not (has_beacon sensors loc) 
    && not (has_sensor sensors loc) 
    && covered_by sensors loc


let parse s =
    trimmed_lines_of s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map Sensor.parse

let find_covered_in_row s r =
    let lb = List.map Sensor.left_bound s |> Base.List.reduce_exn ~f:min in
    let rb = List.map Sensor.right_bound s |> Base.List.reduce_exn ~f:max in
    List.filter (fun x -> is_invalid s (x,r)) (lb @.. rb)
    |> List.length

let part1 (s:string) =
    let sensors = parse s in
    find_covered_in_row sensors 2000000

(* TOO SLOW *)
let search_space lb ub =
    let open Base in
    let range = lb @.. ub in
    Sequence.cartesian_product (Sequence.of_list range) (Sequence.of_list range)

(* TOO SLOW *)
let full_coverage sensors =
    List.map Sensor.coverage_range sensors
    |> Base.List.reduce_exn ~f:Sets.IntPairSet.union


let default_ub = 4000000

let tuning_frequency (x,y) =
    x * 4000000 + y

let find_initial_candidates ?(lb=0) ?(ub=default_ub) sensors =
    let candidates = 
        List.concat_map Sensor.candidate_segments sensors 
        |> List.filter_map (fun a -> Segment.trim_to_fit a lb ub lb ub)
    in
    candidates

let find_candidates ?(lb = 0) ?(ub = default_ub) sensors =
    let candidates = 
        find_initial_candidates ~lb ~ub sensors
    in
    let f acc sensor =
        List.concat_map (Sensor.occlude sensor) acc
    in
    ListLabels.fold_left ~f:f ~init:candidates sensors

let find_empty ?(ub = default_ub) sensors =
    let candidates = find_candidates ~ub sensors in
    let points = List.concat_map Segment.points candidates in
    List.find (fun p -> List.for_all (fun s -> not @@ Sensor.covers s p) sensors) points

let part2 (s:string) =
    let sensors = parse s in
    find_empty sensors
    |> tuning_frequency
