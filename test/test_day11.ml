open! Base
open! Lib.Day11
open! Utils

let test_input = Stdio.In_channel.read_all "../inputs/day11.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_monkey_str = box_trim {|
   Monkey 0:
   Starting items: 52, 78, 79, 63, 51, 94
   Operation: new = old * 13
   Test: divisible by 5
     If true: throw to monkey 1
     If false: throw to monkey 6
|}


let%test_unit "parse_monkey" =
    let m = parse_monkey example_monkey_str in
    [%test_result: int] ~expect:0 m.num;
    [%test_result: int list] ~expect:[52;78;79;63;51;94] m.holding;
    [%test_result: int] ~expect:(2*13) (m.op 2);
    [%test_result: bool] ~expect:true (m.test 10);
    [%test_result: bool] ~expect:false (m.test 27);
    [%test_result: int] ~expect:1 (m.true_target);
    [%test_result: int] ~expect:6 (m.false_target)

let example_monkeys_str = {|
Monkey 0:
Starting items: 79, 98
Operation: new = old * 19
Test: divisible by 23
  If true: throw to monkey 2
  If false: throw to monkey 3

Monkey 1:
Starting items: 54, 65, 75, 74
Operation: new = old + 6
Test: divisible by 19
  If true: throw to monkey 2
  If false: throw to monkey 0

Monkey 2:
Starting items: 79, 60, 97
Operation: new = old * old
Test: divisible by 13
  If true: throw to monkey 1
  If false: throw to monkey 3

Monkey 3:
Starting items: 74
Operation: new = old + 3
Test: divisible by 17
  If true: throw to monkey 0
  If false: throw to monkey 1
|}

let print_rounds n =
    let example_monkeys = parse_monkeys example_monkeys_str in
    let results = process_rounds n example_monkeys in
    Array.iter ~f:(fun {inspections;_} -> Stdio.printf "%d\n" inspections) results

let%expect_test "part1" =
    print_rounds 20;
    [%expect {|
        101
        95
        7
        105
    |}];
    Stdio.printf "%d\n" (part1 example_monkeys_str);
    [%expect {| 10605 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 58786 |}]

let print_rounds' n =
    let example_monkeys = parse_monkeys example_monkeys_str in
    let results = process_rounds' n example_monkeys in
    Array.iter ~f:(fun {inspections;_} -> Stdio.printf "%d\n" inspections) results

let%expect_test "part2" =
    print_rounds' 1;
    [%expect {|
      2
      4
      3
      6 |}];
    print_rounds' 20;
    [%expect {|
      99
      97
      8
      103 |}];
    print_rounds' 1000;
    [%expect {|
      5204
      4792
      199
      5192 |}];
    print_rounds' 2000;
    [%expect {|
      10419
      9577
      392
      10391 |}];
    print_rounds' 3000;
    [%expect {|
      15638
      14358
      587
      15593 |}];
    print_rounds' 4000;
    [%expect {|
      20858
      19138
      780
      20797 |}];
    Stdio.printf "%d\n" (part2 example_monkeys_str);
    [%expect {| 2713310158 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 14952185856 |}]
