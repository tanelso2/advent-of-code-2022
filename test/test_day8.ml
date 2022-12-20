open! Base
open! Lib.Day8
open! Utils

let test_input = Stdio.In_channel.read_all "../inputs/day8.txt"

let example = box_trim {|
    30373
    25512
    65332
    33549
    35390
|}

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 example;
    [%expect {| 21 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1801 |}]

let%expect_test "part2" =
    [%test_result: int] ~expect:4 @@ find_scenic_score (parse example) (2,1);
    [%test_result: int] ~expect:8 @@ find_scenic_score (parse example) (2,3);
    [%test_result: int] ~expect:8 @@ find_best_scenic_score (parse example);
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 209880 |}]
