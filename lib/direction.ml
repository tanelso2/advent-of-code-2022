type dir = Up | Down | Left | Right

type t = dir

let turn_left = function
| Left -> Down
| Down -> Right
| Right -> Up
| Up -> Left

let turn_right = function
| Up -> Right
| Right -> Down
| Down -> Left
| Left -> Up

let turn_around = function
| Up -> Down
| Down -> Up
| Left -> Right
| Right -> Left

let dx_dy = function
| Up -> (0, -1)
| Down -> (0, 1)
| Left -> (-1, 0)
| Right -> (1, 0)

let move (x,y) dir =
  let (dx,dy) = dx_dy dir in
  (x+dx, y+dy)

let of_char = function
| 'U' -> Up
| 'D' -> Down
| 'R' -> Right
| 'L' -> Left
| _ -> failwith "not a valid direction char"

let of_string = function
| "U" -> Up
| "D" -> Down
| "R" -> Right
| "L" -> Left
| _ -> failwith "not a valid direction string"

let is_below (_,y1) (_,y2) =
  y1 > y2

let is_above (_,y1) (_,y2) =
  y1 < y2

let is_to_right_of (x1,_) (x2,_) =
  x1 > x2

let is_to_left_of (x1,_) (x2,_) =
  x1 < x2

let (@->?) start finish =
  let tests_and_dirs_to_go = [(is_below, Up);
                              (is_above,Down);
                              (is_to_left_of,Right);
                              (is_to_right_of,Left)]
  in
  let dirs = 
    tests_and_dirs_to_go
    |> List.filter (fun (f,_) -> f start finish)
    |> List.map snd
  in
  match dirs with
  | [] -> `Same
  | [d] -> `Card d
  | [d1;d2] -> `Diag (d1, d2)
  | _ -> failwith "Error, more than two tests passed?"
