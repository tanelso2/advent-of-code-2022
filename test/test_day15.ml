open! Base
open! Lib.Day15
open! Utils
open! Geom

let test_input = Stdio.In_channel.read_all "../inputs/day15.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    Sensor at x=9, y=16: closest beacon is at x=10, y=16
    Sensor at x=13, y=2: closest beacon is at x=15, y=3
    Sensor at x=12, y=14: closest beacon is at x=10, y=16
    Sensor at x=10, y=20: closest beacon is at x=10, y=16
    Sensor at x=14, y=17: closest beacon is at x=10, y=16
    Sensor at x=8, y=7: closest beacon is at x=2, y=10
    Sensor at x=2, y=0: closest beacon is at x=2, y=10
    Sensor at x=0, y=11: closest beacon is at x=2, y=10
    Sensor at x=20, y=14: closest beacon is at x=25, y=17
    Sensor at x=17, y=20: closest beacon is at x=21, y=22
    Sensor at x=16, y=7: closest beacon is at x=15, y=3
    Sensor at x=14, y=3: closest beacon is at x=15, y=3
    Sensor at x=20, y=1: closest beacon is at x=15, y=3
|}

let%expect_test "part1" =
    let sensors = parse example in
    Stdio.printf "%d\n" (find_covered_in_row sensors 10);
    [%expect {| 26 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 5461729 |}]

let%test_unit "coverage_range" =
    let open Sensor in
    let open Lib.Sets in
    let s = {sensor=0,0;beacon=1,0} in
    [%test_result: int] ~expect:1 @@ radius s;
    let range = coverage_range s in
    [%test_result: int] ~expect:5 @@ IntPairSet.cardinal range

let%expect_test "Segment.trim_to_fit" =
    let test_segment = Segment.from_points (0,0) (20,20) in
    let s' = Segment.trim_to_fit test_segment 0 20 0 20 in
    (match s' with
    | Some (Segment ((a,b),(c,d),_)) ->
        Stdio.printf "(%d,%d) (%d,%d)\n" a b c d 
    | _ -> ());
    [%expect {|(0,0) (20,20)|}]

let%expect_test "part2" =
    let sensors = parse example in
    let empty = find_empty ~ub:20 sensors in
    Stdio.printf "%d,%d\n" (fst empty) (snd empty);
    [%expect {| 14,11 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 10621647166538 |}]
