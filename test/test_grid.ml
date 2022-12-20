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
