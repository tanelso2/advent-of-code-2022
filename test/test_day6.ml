open! Base
open! Lib.Day6

let test_input = Stdio.In_channel.read_all "../inputs/day6.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let examples = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7);
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5);
    ("nppdvjthqldpwncqszvftbrmjlhg", 6);
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10);
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)
]

let test_example (input, expected) =
    [%test_result: int] (part1 input) ~expect:expected

let print_result (input, _) =
    Stdio.printf "%d\n" (part1 input)

let%expect_test "part1" =
    (* List.iter ~f:print_result examples;
    [%expect {||}]; *)
    List.iter ~f:test_example examples;
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1134 |}]

let part2_examples = [
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19);
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23);
    ("nppdvjthqldpwncqszvftbrmjlhg", 23);
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29);
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)
]

let test_example2 (input, expected) =
    [%test_result: int] (part2 input) ~expect:expected

let%expect_test "part2" =
    List.iter ~f:test_example2 part2_examples;
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 2263 |}]
