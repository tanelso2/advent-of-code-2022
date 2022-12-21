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