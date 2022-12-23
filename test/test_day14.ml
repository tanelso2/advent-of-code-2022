open! Base
open! Lib.Day14
open! Utils

let test_input = Stdio.In_channel.read_all "../inputs/day14.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = box_trim {|
    498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9
|}

let%expect_test "parsing" =
    let ls = parse_lines example in
    Stdio.printf "%s\n" @@ Sexp.to_string @@ [%sexp_of: ((int * int) * (int * int)) list] ls;
    [%expect {| (((498 4)(498 6))((498 6)(496 6))((503 4)(502 4))((502 4)(502 9))((502 9)(494 9))) |}]

let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example;
    [%expect {| 24 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1061 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 example;
    [%expect {| 93 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 25055 |}]
