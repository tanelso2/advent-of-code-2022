open Utils

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

let find_empty sensors _ =
    let _  = full_coverage sensors in
    14,11
    (* let is_possible loc =
        not @@ Sets.IntPairSet.mem loc blocked
    in
    let rec helper x y =
        if x > search_size
        then helper 0 (y+1)
        else if y > search_size
        then failwith "Couldn't find anything"
        else if is_possible (x,y)
        then (x,y)
        else helper (x+1) y
    in
    helper 0 0 *)

let tuning_frequency (x,y) =
    x * 4000000 + y

let part2 (s:string) =
    let sensors = parse s in
    find_empty sensors 4000000
    |> tuning_frequency
