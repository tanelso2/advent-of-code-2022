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

let%expect_test "parsing exploration" =
    let (num,items,op,div,tt,ft, test) = test_fn example_monkey_str in
    Stdio.printf "%d\n" num;
    [%expect {| 0 |}];
    Stdio.printf "%s\n" @@ Sexp.to_string ([%sexp_of: int list] items);
    [%expect {| (52 78 79 63 51 94) |}];
    Stdio.printf "%d\n" (op 3);
    [%expect {| 39 |}];
    Stdio.printf "%d\n" div;
    [%expect {| 5 |}];
    Stdio.printf "%d\n" tt;
    [%expect {| 1 |}];
    Stdio.printf "%d\n" ft;
    [%expect {| 6 |}];
    Stdio.printf "%b\n" (test 10);
    [%expect {| true |}];
    Stdio.printf "%b\n" (test 7);
    [%expect {| false |}];
    let monkeys = parse_monkeys test_input in
    Stdio.printf "%d\n" @@ List.length monkeys;
    [%expect {| 8 |}];
    let first_monkey = List.hd_exn monkeys in
    Stdio.printf "%d\n" @@ first_monkey.num;
    [%expect {| 0 |}];
    let last_monkey = List.last_exn monkeys in
    Stdio.printf "%d\n" @@ last_monkey.num;
    [%expect {| 7 |}];
    Stdio.printf "%d\n" @@ last_monkey.op 2;
    [%expect {| 4 |}]

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
    [%expect {||}];
    print_rounds' 20;
    [%expect {||}];
    Stdio.printf "%d\n" (part2 example_monkeys_str);
    [%expect {| 2713310158 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| |}]
