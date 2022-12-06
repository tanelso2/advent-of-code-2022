open! Base

open! Lib.Utils

let example = {|



      ABC
      DEF
      GHI



|}

let%test_unit "box_trim" =
  let box = box_trim example in
  let lines = lines_of box in
  [%test_result: int] ~expect:3 (List.length lines);
  List.iter ~f:(fun l ->
    [%test_result: int] ~expect:3 (String.length l)
    ) lines
