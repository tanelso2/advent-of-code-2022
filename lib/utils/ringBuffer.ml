type 'a t = {
  arr: 'a array;
  length: int;
  idx: int;
}

let get {arr; length; idx} =
  let v = arr.(idx) in
  let idx' = (idx + 1) mod length in
  (v, {arr = arr; length = length; idx = idx'})

let get_val (rb: 'a t ref) : 'a =
  let (v, rb') = get !rb in
  rb := rb';
  v

let of_array a =
  let arr = Array.copy a in
  let length = Array.length a in
  let idx = 0 in
  {arr; length; idx}