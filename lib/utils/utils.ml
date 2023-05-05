include StringUtils
module StringUtils = StringUtils

module Heap = Heap
module Heaps = Heaps

include ListUtils
module ListUtils = ListUtils

include FunUtils
include FunUtils.Ops
module FunUtils = FunUtils

include RefUtils
module RefUtils = RefUtils

module Stack = struct
  include Stack
  let pop_n s i = 
      let rec helper s i acc =
          if i <= 0
          then acc
          else 
              let x = Stack.pop s in
              helper s (i - 1) (x::acc)
      in
      helper s i []
end

module RegexUtils = RegexUtils

module IntPair = IntPair

module StringPair = StringPair 

module Map = struct
  open! Base
  include Map
  let max_by (m: ('k, 'v, _) Map.t) ~key_fn:(f:('k -> 'v -> int)): ('k * 'v) option =
    let elem = ref None in
    let max_val = ref Int.min_value in
    let iter_fn ~key:k ~data:v =
      let curr_val = f k v in
      if curr_val > !max_val
      then
        (max_val := curr_val;
        elem := (Some (k, v)))
      else ()
    in
    let () = Map.iteri m ~f:iter_fn in
    !elem

  let max_by_exn m ~key_fn:f =
    match max_by m ~key_fn:f with
    | Some x -> x
    | None -> failwith "max_by_exn failed"

  let min_by m ~key_fn:f = max_by m ~key_fn:(fun k v -> f k v |> Int.neg)

  let min_by_exn m ~key_fn:f =
    match min_by m ~key_fn:f with
    | Some x -> x
    | None -> failwith "min_by_exn failed"
end