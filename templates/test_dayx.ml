open! Base
open! Lib.Day{{day}}

let test_input = Stdio.In_channel.read_all "../inputs/day{{day}}.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "part1" =
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}]

let%expect_test "part2" =
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}]
