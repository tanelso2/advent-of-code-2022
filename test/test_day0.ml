open! Base
open! Lib.Day0

let test_input = Stdio.In_channel.read_all "../inputs/day0.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)
let example = Caml.String.trim {|
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
|}

let%expect_test "part1" =
    Stdio.print_endline "3";
    [%expect {| 3 |}];
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 7 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1387 |}]

let%expect_test "part2" =
    Stdio.print_endline "2";
    [%expect {| 2 |}];
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 5 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1362 |}]
