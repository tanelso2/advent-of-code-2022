open! Base
open! Lib.Day13

let test_input = Stdio.In_channel.read_all "../inputs/day13.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_packet = {| [1,1,3,1,1] |}

let example_packets = Utils.box_trim {|
    [1,1,3,1,1]
    [1,1,5,1,1]

    [[1],[2,3,4]]
    [[1],4]

    [9]
    [[8,7,6]]
|}

let full_example = Utils.box_trim {|
    [1,1,3,1,1]
    [1,1,5,1,1]

    [[1],[2,3,4]]
    [[1],4]

    [9]
    [[8,7,6]]

    [[4,4],4,4]
    [[4,4],4,4,4]

    [7,7,7,7]
    [7,7,7]

    []
    [3]

    [[[]]]
    [[]]

    [1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]
|}

let%expect_test "parsing test" =
    let print_packet s =
        s
        |> Packet.parse
        |> Packet.sexp_of_t
        |> Sexp.to_string
        |> Stdio.printf "%s\n"
    in
    print_packet example_packet;
    [%expect {| (PList((PVal 1)(PVal 1)(PVal 3)(PVal 1)(PVal 1))) |}];
    let lines = Utils.trimmed_lines_of example_packets |> List.filter ~f:(fun x -> String.length x > 0) in
    List.iter ~f:print_packet lines;
    [%expect {|
      (PList((PVal 1)(PVal 1)(PVal 3)(PVal 1)(PVal 1)))
      (PList((PVal 1)(PVal 1)(PVal 5)(PVal 1)(PVal 1)))
      (PList((PList((PVal 1)))(PList((PVal 2)(PVal 3)(PVal 4)))))
      (PList((PList((PVal 1)))(PVal 4)))
      (PList((PVal 9)))
      (PList((PList((PVal 8)(PVal 7)(PVal 6))))) 
    |}];
    let pairs = parse_pairs example_packets in
    Stdio.printf "%d pairs\n" @@ List.length pairs;
    [%expect {| 3 pairs |}];
    let pairs = parse_pairs full_example in
    Stdio.printf "%d pairs\n" @@ List.length pairs;
    [%expect {| 8 pairs |}]

let%expect_test "part1" =
    Stdio.printf "%d\n" @@ part1 full_example;
    [%expect {| 13 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 5366 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" @@ part2 full_example;
    [%expect {| 140 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 23391 |}]
