open! Base
open Utils

let double l = List.map ~f:(fun x -> x,x) l


let%test_unit "minHeap" =
  let module Heap = Heaps.IntMinHeap in
  let h = Heap.of_list @@ double [3;4;1;2] in
  [%test_result: int] ~expect:4 @@ Heap.size h;
  [%test_result: int option] ~expect:(Some 1) @@ Heap.peek h;
  [%test_result: int list] ~expect:[1;2;3;4] @@ Heap.take_all h

let%test_unit "maxHeap" =
  let module Heap = Heaps.IntMaxHeap in
  let h = Heap.of_list @@ double [3;4;1;2] in
  [%test_result: int] ~expect:4 @@ Heap.size h;
  [%test_result: int option] ~expect:(Some 4) @@ Heap.peek h;
  [%test_result: int list] ~expect:[4;3;2;1] @@ Heap.take_all h

let%test_unit "resizing" =
  let module Heap = Heaps.IntMinHeap in
  let h = Heap.create ~size:1 () in
  Heap.add_all h @@ double [3;4;1;2;6;7;8;5];
  [%test_result: int option] ~expect:(Some 1) @@ Heap.peek h;
  [%test_result: int list] ~expect:[1;2;3;4;5;6;7;8] @@ Heap.take_all h;
  let h = Heap.create ~size:1 () in
  Heap.add_all h @@ double [2;1;3;4];
  [%test_result: int option] ~expect:(Some 1) @@ Heap.pop h;
  [%test_result: int option] ~expect:(Some 2) @@ Heap.pop h;
  Heap.add_all h @@ double [5;6;2;7;1];
  [%test_result: int list] ~expect:[1;2;3;4;5;6;7] @@ Heap.take_all h

let%test_unit "duplicates" =
  let module Heap = Heaps.IntMinHeap in
  let h = Heap.create () in
  Heap.add_all h @@ double [1;2;1;3;3;1;4;5];
  [%test_result: int list] ~expect:[1;1;1;2;3;3;4;5] @@ Heap.take_all h
