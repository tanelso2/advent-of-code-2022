open! Base
open! Lib.Day5

open! Lib.Utils

let test_input = Stdio.In_channel.read_all "../inputs/day5.txt"

let part1' () = Option.try_with (fun () -> part1 test_input)

let part2' () = Option.try_with (fun () -> part2 test_input)

let example = box_trim {|
        [D]   
    [N] [C]    
    [Z] [M] [P]
     1   2   3 

    move 1 from 2 to 1
    move 3 from 1 to 3
    move 2 from 2 to 1
    move 1 from 1 to 2
|}

let%expect_test "parse_box_line" =
    List.iter ~f:(fun (i,x) -> Stdio.printf "%d %c\n" i x) @@ parse_box_line "[Z] [M] [P]";
    [%expect {|
      3 P
      2 M
      1 Z |}];
    List.iter ~f:(fun (i,x) -> Stdio.printf "%d %c\n" i x) @@ parse_box_line "[C] [H]     [T] [T] [G] [B] [Z] [B]";
    [%expect {|
      9 B
      8 Z
      7 B
      6 G
      5 T
      4 T
      2 H
      1 C |}]

let%expect_test "parse_boxes" =
    let (boxes,_) = parse example in
    [%test_result: int] ~expect:3 (Array.length boxes);
    Stdio.printf "%d\n" (Caml.Stack.length boxes.(0));
    Stdio.printf "%d\n" (Caml.Stack.length boxes.(1));
    Stdio.printf "%d\n" (Caml.Stack.length boxes.(2));
    [%expect {|
        2
        3
        1
    |}]

let%test_unit "do_intruction" =
    let open Caml.Stack in
    let src_stack = create () in
    let dest_stack = create () in
    let stacks = [| src_stack; dest_stack |] in
    push 'a' src_stack;
    [%test_result: int] ~expect:1 (length src_stack);
    [%test_result: int] ~expect:0 (length dest_stack);
    do_instruction stacks {amount = 1; src = 1; dest = 2};
    [%test_result: int] ~expect:0 (length src_stack);
    [%test_result: int] ~expect:1 (length dest_stack)

let%expect_test "part1" =
    Stdio.print_endline (part1 example);
    [%expect {| CMZ |}];
    (match part1' () with
     | None -> ()
     | Some x -> Stdio.printf "%s\n" x);
    [%expect {| CFFHVVHNC |}]

let%expect_test "part2" =
    Stdio.print_endline (part2 example);
    [%expect {| MCD |}];
    (match part2' () with
     | None -> ()
     | Some x -> Stdio.printf "%s\n" x);
    [%expect {| FSZWBPTBG |}]
