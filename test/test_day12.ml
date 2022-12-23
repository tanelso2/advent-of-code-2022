open! Base
open! Lib
open! Day12

let test_input = Stdio.In_channel.read_all "../inputs/day12.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    Sabqponm
    abcryxxl
    accszExk
    acctuvwj
    abdefghi
|}

let%expect_test "parsing test" =
    let (sp,ep,g) = parse example in
    let (sx,sy) = sp in
    let (ex,ey) = ep in
    Stdio.printf "starting_point is (%d,%d) and has height: %d\n" sx sy (Grid.get_space_exn g sp);
    [%expect {| starting_point is (0,0) and has height: 0 |}];
    Stdio.printf "ending_point is (%d,%d) and has height: %d\n" ex ey (Grid.get_space_exn g ep);
    [%expect {| ending_point is (5,2) and has height: 25 |}]


let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example;
    [%expect {| 31 |}];
    Stdio.printf "%d\n" @@ part1 test_input;
    [%expect {| 383 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 383 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 example;
    [%expect {| 29 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 377 |}]
