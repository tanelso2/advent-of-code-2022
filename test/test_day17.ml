open! Base
open! Lib.Day17

let test_input = Stdio.In_channel.read_all "../inputs/day17.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {||}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}]

let%expect_test "part2" =
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}]
