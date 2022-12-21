let (<<) f g x = f(g(x))

let (<<..) f g x y = f(g x y)

let apply_n_times ~init ~f n =
  let rec helper acc i =
    if i >= n
    then acc
    else helper (f acc) (i + 1)
  in
  helper init 0