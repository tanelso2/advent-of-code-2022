open! Base
open! Lib.Day9

let example = {|
    R 4
    U 4
    L 3
    D 1
    R 4
    D 1
    L 5
    R 2
|}

let bigger_example = {|
    R 5
    U 8
    L 8
    D 3
    R 17
    D 10
    L 25
    U 20
|}

let test_input = Stdio.In_channel.read_all "../inputs/day9.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 13 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 6406 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 1 |}];
    Stdio.printf "%d\n" (part2 bigger_example);
    [%expect {| 36 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 2643 |}]
