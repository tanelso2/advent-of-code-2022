open! Base
open! Lib.Day4

let test_input = Stdio.In_channel.read_all "../inputs/day4.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    2-4,6-8
    2-3,4-5
    5-7,7-9
    2-8,3-7
    6-6,4-6
    2-6,4-8
|}

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 2 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 490 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 4 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 921 |}]
