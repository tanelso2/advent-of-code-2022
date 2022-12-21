open! Base
open! Lib.Day10

let test_input = Stdio.In_channel.read_all "../inputs/day10.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = {|
    addx 15
    addx -11
    addx 6
    addx -3
    addx 5
    addx -1
    addx -8
    addx 13
    addx 4
    noop
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx -35
    addx 1
    addx 24
    addx -19
    addx 1
    addx 16
    addx -11
    noop
    noop
    addx 21
    addx -15
    noop
    noop
    addx -3
    addx 9
    addx 1
    addx -3
    addx 8
    addx 1
    addx 5
    noop
    noop
    noop
    noop
    noop
    addx -36
    noop
    addx 1
    addx 7
    noop
    noop
    noop
    addx 2
    addx 6
    noop
    noop
    noop
    noop
    noop
    addx 1
    noop
    noop
    addx 7
    addx 1
    noop
    addx -13
    addx 13
    addx 7
    noop
    addx 1
    addx -33
    noop
    noop
    noop
    addx 2
    noop
    noop
    noop
    addx 8
    noop
    addx -1
    addx 2
    addx 1
    noop
    addx 17
    addx -9
    addx 1
    addx 1
    addx -3
    addx 11
    noop
    noop
    addx 1
    noop
    addx 1
    noop
    noop
    addx -13
    addx -19
    addx 1
    addx 3
    addx 26
    addx -30
    addx 12
    addx -1
    addx 3
    addx 1
    noop
    noop
    noop
    addx -9
    addx 18
    addx 1
    addx 2
    noop
    noop
    addx 9
    noop
    noop
    noop
    addx -1
    addx 2
    addx -37
    addx 1
    addx 3
    noop
    addx 15
    addx -21
    addx 22
    addx -6
    addx 1
    noop
    addx 2
    addx 1
    noop
    addx -10
    noop
    noop
    addx 20
    addx 1
    addx 2
    addx 2
    addx -6
    addx -11
    noop
    noop
    noop
|}

let%expect_test "part1" =
    let v = example |> parse |> handle_cmds |> find_important_states in
    List.iter ~f:(fun {x_val;time} -> Stdio.printf "x_val: %d, time: %d\n" x_val time) v;
    [%expect {|
      x_val: 21, time: 20
      x_val: 19, time: 60
      x_val: 18, time: 100
      x_val: 21, time: 140
      x_val: 16, time: 180
      x_val: 18, time: 219 |}];
    Stdio.printf "%d\n" (part1 example);
    [%expect {| 13140 |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%d\n" x);
    [%expect {| 10760 |}]

let expected_output = {|
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######.....
|}

let%expect_test "part2" =
    let states = handle_cmds @@ parse example in
    let all_states = state_at_each_cycle states in
    let example_states = List.take all_states 21 in
    List.iter ~f:(fun {x_val;time} -> 
                    let draw = should_draw {x_val;time} in
                    Stdio.printf "time: %d, x_val: %d, should_draw: %b\n" time x_val draw
                 )
              example_states;
    [%expect {|
      time: 1, x_val: 1, should_draw: true
      time: 2, x_val: 1, should_draw: true
      time: 3, x_val: 16, should_draw: false
      time: 4, x_val: 16, should_draw: false
      time: 5, x_val: 5, should_draw: true
      time: 6, x_val: 5, should_draw: true
      time: 7, x_val: 11, should_draw: false
      time: 8, x_val: 11, should_draw: false
      time: 9, x_val: 8, should_draw: true
      time: 10, x_val: 8, should_draw: true
      time: 11, x_val: 13, should_draw: false
      time: 12, x_val: 13, should_draw: false
      time: 13, x_val: 12, should_draw: true
      time: 14, x_val: 12, should_draw: true
      time: 15, x_val: 4, should_draw: false
      time: 16, x_val: 4, should_draw: false
      time: 17, x_val: 17, should_draw: true
      time: 18, x_val: 17, should_draw: true
      time: 19, x_val: 21, should_draw: false
      time: 20, x_val: 21, should_draw: false
      time: 21, x_val: 21, should_draw: true 
    |}];
    Stdio.printf "%s\n" (draw_cycles all_states);
    [%expect {|
        ##..##..##..##..##..##..##..##..##..##..
        ###...###...###...###...###...###...###.
        ####....####....####....####....####....
        #####.....#####.....#####.....#####.....
        ######......######......######......####
        #######.......#######.......#######.....
    |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%s\n" x);
    [%expect {|
      ####.###...##..###..#..#.####..##..#..#.
      #....#..#.#..#.#..#.#..#.#....#..#.#..#.
      ###..#..#.#....#..#.####.###..#....####.
      #....###..#.##.###..#..#.#....#.##.#..#.
      #....#....#..#.#....#..#.#....#..#.#..#.
      #....#.....###.#....#..#.#.....###.#..#. 
    |}]
