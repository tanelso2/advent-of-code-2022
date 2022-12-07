open! Base
open! Lib.Day1
open Stdio
open Utils

let test_input = Stdio.In_channel.read_all "../inputs/day1.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|}

let%expect_test "part1" =
    Stdio.printf "%d\n" (List.length @@ lines_of example);
    [%expect {| 16 |}];
    Stdio.printf "%d\n" (List.length @@ parse example);
    [%expect {| 5 |}];
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 24000 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 71780 |}]

let%expect_test "part2" =
    printf "%d\n" (part2 example);
    [%expect {| 45000 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 212489 |}]
