open! Base
open! Utils

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

let%test_unit "@.." =
    [%test_result: int list] ~expect:[0] @@ 0 @.. 0;
    [%test_result: int list] ~expect:[0;1] @@ 0 @.. 1;
    [%test_result: int list] ~expect:[] @@ 1 @.. 0;
    ()

let%test_unit "apply_n_times" =
    [%test_result: int] ~expect:1 @@ apply_n_times ~init:0 ~f:((+) 1) 1;
    [%test_result: int] ~expect:3 @@ apply_n_times ~init:0 ~f:((+) 1) 3