open! Base
open Utils

let%expect_test "ring_buffer" =
  let module RB = RingBuffer in
  let rb = ref (RB.of_array [| 1; 2; 3 |]) in
  List.range 0 5 
  |> List.iter ~f:(fun _ -> Stdio.printf "%d\n" (RB.get_val rb));
  [%expect {|
    1
    2
    3
    1
    2
  |}]