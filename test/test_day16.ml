open! Base
open! Lib.Day16

let test_input = Stdio.In_channel.read_all "../inputs/day16.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example_valve = "Valve TM has flow rate=3; tunnels lead to valves GU, KQ, BV, MK" 

let example_singular_valve = "Valve JJ has flow rate=21; tunnel leads to valve II"

let%expect_test "parsing" =
    let v = Valve.parse example_valve in
    [%test_result: string] ~expect:"TM" v.name;
    [%test_result: int] ~expect:3 v.flow_rate;
    [%test_result: int] ~expect:4 @@ List.length v.tunnels;
    v.tunnels |> String.concat ~sep:", " |> Stdio.printf "%s\n";
    [%expect {| GU, KQ, BV, MK |}];
    let v2 = Valve.parse example_singular_valve in
    [%test_result: string] ~expect:"JJ" v2.name;
    [%test_result: int] ~expect:21 v2.flow_rate;
    [%test_result: int] ~expect:1 @@ List.length v2.tunnels;
    v2.tunnels |> String.concat ~sep:", " |> Stdio.printf "%s\n";
    [%expect {| II |}]

let example = {|
    Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
    Valve BB has flow rate=13; tunnels lead to valves CC, AA
    Valve CC has flow rate=2; tunnels lead to valves DD, BB
    Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
    Valve EE has flow rate=3; tunnels lead to valves FF, DD
    Valve FF has flow rate=0; tunnels lead to valves EE, GG
    Valve GG has flow rate=0; tunnels lead to valves FF, HH
    Valve HH has flow rate=22; tunnel leads to valve GG
    Valve II has flow rate=0; tunnels lead to valves AA, JJ
    Valve JJ has flow rate=21; tunnel leads to valve II
|}

let num_with_flow (vs: Valve.t list) : int =
    let open Valve in
    vs 
    |> List.filter ~f:(fun x -> x.flow_rate > 0)
    |> List.length

let print_num_with_flow vs : unit =
    num_with_flow vs |>
    Stdio.printf "Found %d valves with flow\n"

let%expect_test "valves with flow" =
    print_num_with_flow (parse example);
    print_num_with_flow (parse test_input);
    [%expect {|
      Found 6 valves with flow
      Found 15 valves with flow 
    |}];
    let vs = parse example in
    let m = floydwarshall vs in
    let print_val i j =
        match Map.find m (i,j) with
        | None -> ()
        | Some v -> Stdio.printf "Value at (%s,%s) is %d\n" i j v
    in
    print_val "AA" "AA";
    print_val "AA" "BB";
    print_val "AA" "CC";
    print_val "AA" "JJ";
    print_val "BB" "JJ";
    [%expect {|
      Value at (AA,AA) is 0
      Value at (AA,BB) is 1
      Value at (AA,CC) is 2
      Value at (AA,JJ) is 2
      Value at (BB,JJ) is 3
    |}];
    let example_score = score_result m vs (Array.of_list ["DD"; "BB"; "JJ"; "HH"; "EE"; "CC"]) in
    Stdio.printf "The score was %d\n" example_score;
    [%expect {|
        The score was 1651
    |}];
    let (best, nodes_left) = find_max_journey vs in
    Stdio.printf "The max_journey was %d\n" best;
    Stdio.printf "There were %d nodes_left\n" (ValveSet.cardinal nodes_left);
    [%expect {|
        The max_journey was 1651
        There were 0 nodes_left
    |}];
    let (best, nodes_left) = find_max_journey (parse test_input) in
    Stdio.printf "The max_journey was %d\n" best;
    Stdio.printf "There were %d nodes_left\n" (ValveSet.cardinal nodes_left);
    [%expect {|
        The max_journey was 1595
        There were 7 nodes_left
    |}]

let%expect_test "part1" =
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 1651 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 1595 |}]

let%expect_test "part2" =
    Stdio.printf "%d\n" (find_max_journies (parse example));
    [%expect {| 1707 |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 2189 |}]
