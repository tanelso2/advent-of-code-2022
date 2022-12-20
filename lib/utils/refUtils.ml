let replace_max r ?(f : (int -> int -> unit) option) v =
  let curr = !r in
  if v > curr 
  then begin
    (match f with
     | None -> ()
     | Some fn -> fn curr v)
    ;
    r := v
  end
  else ()

let replace_min r ?(f : (int -> int -> unit) option) v =
  let curr = !r in
  if v < curr 
  then begin
    (match f with
     | None -> ()
     | Some fn -> fn curr v);
    r := v
  end
  else ()