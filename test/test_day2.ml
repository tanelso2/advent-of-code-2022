open! Base
open! Lib.Day2

let test_input = Stdio.In_channel.read_all "../inputs/day2.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    A Y
    B X
    C Z
|}

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 15 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 15572 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 12 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 16098 |}]
