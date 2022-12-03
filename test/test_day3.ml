open! Base
open! Lib.Day3

let test_input = Stdio.In_channel.read_all "../inputs/day3.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw
|}

let%test_unit "split_in_half" =
    let l = [1;2;3;4] in
    let (a,b) = split_in_half l in
    [%test_eq: int] (List.length a) (List.length b)

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 157 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 7845 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (part2 example);
    [%expect {| 70 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 2790 |}]
