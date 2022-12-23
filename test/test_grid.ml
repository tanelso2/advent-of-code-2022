open! Base
open! Lib.Grid
open! Utils

let example = box_trim {|
  AB
  CD
|}

let example2 = box_trim {|
  ABC
  DEF
  GHI
|}

let g = parse example Fn.id

let g2 = parse example2 Fn.id

let%test_unit "rows" =
  let rs = rows g in
  [%test_result: char list list] ~expect:[['A';'B'];['C';'D']] rs

let%test_unit "columns" =
  let cs = columns g in
  [%test_result: char list list] ~expect:[['A';'C'];['B';'D']] cs

let%test_unit "to_left_of" =
  [%test_result: ((int * int) * char) list] ~expect:[] @@ to_left_of g (0,0);
  let b = (1,0) in
  let expected_b = [((0,0), 'A')] in
  [%test_result: ((int * int) * char) list] ~expect:expected_b @@ to_left_of g b;
  let expected = [((1,0), 'B'); ((0,0), 'A')] in
  [%test_result: ((int * int) * char) list] ~expect:expected @@ to_left_of g2 (2,0)

let%test_unit "to_right_of" =
  [%test_result: ((int * int) * char) list] ~expect:[((1,0), 'B')] @@ to_right_of g (0,0);
  [%test_result: ((int * int) * char) list] ~expect:[(1,0), 'B'; (2,0), 'C'] @@ to_right_of g2 (0,0);
  ()

let%test_unit "above" =
  [%test_result: ((int * int) * char) list] ~expect:[] @@ above g (0,0);
  [%test_result: ((int * int) * char) list] ~expect:[(0,1), 'D'; (0,0), 'A'] @@ above g2 (0,2);
  ()

let%test_unit "below" =
  [%test_result: ((int * int) * char) list] ~expect:[((0,1), 'C')] @@ below g (0,0);
  [%test_result: ((int * int) * char) list] ~expect:[(0,1), 'D'; (0,2), 'G'] @@ below g2 (0,0);
  ()

let%expect_test "line_from" =
  let short_line = line_from g (0,0) (0,0) in
  [%test_result: ((int * int) * char) list] ~expect:[((0,0), 'A')] short_line;
  let from_origin = line_from g2 (0,0) in
  let length_from_origin = from_origin >> List.length in
  [%test_result: int] ~expect:2 @@ length_from_origin (1,0);
  [%test_result: int] ~expect:3 @@ length_from_origin (2,0);
  [%test_result: int] ~expect:2 @@ length_from_origin (0,1);
  [%test_result: int] ~expect:3 @@ length_from_origin (0,2);
  let to_string (l: ((int * int) * char) list)  =
    l
    |> List.map ~f:snd
    |> Base.String.of_char_list
  in
  let print_line s f =
    line_from g2 s f |> to_string |> Stdio.printf "%s\n"
  in
  print_line (0,0) (2,0);
  [%expect {| ABC |}];
  print_line (2,0) (0,0);
  [%expect {| CBA |}];
  print_line (0,0) (0,2);
  [%expect {| ADG |}];
  print_line (0,2) (0,0);
  [%expect {| GDA |}];
  ()