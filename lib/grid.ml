open Utils

type 'a t = 'a array array

type loc = int * int

let empty_grid w h init =
  Array.make_matrix w h init
and width grid =
  Array.get grid 0 |> Array.length
and height grid =
  Array.length grid

let parse s (f: char -> 'a) =
  let s = String.trim s in
  let lines = lines_of s |> List.map String.trim in
  let array_of_string s = s |> String.to_seq |> Array.of_seq in
  let g = Array.of_list lines |> Array.map (fun s -> array_of_string s |> Array.map f) in
  g

let to_string g (f: 'a -> char) : string =
  let show_row r = Array.map f r 
                   |> Array.to_seq
                   |> String.of_seq
  in
  String.concat "\n" (g |> Array.to_list |> List.map show_row)

let upper_right grid = (width grid - 1, 0)

let get_space grid (x,y) =
  let w = width grid in
  let h = height grid in
  if x < 0 || x >= w || y < 0 || y >= h
  then None
  else Some (Array.get grid y |> Fun.flip Array.get x)

let get_space_exn grid loc =
  match get_space grid loc with
  | None -> failwith "get_space_exn failed"
  | Some v -> v

let space_and_loc_exn grid loc =
  (loc, get_space_exn grid loc)

let iter f g =
    Array.iter (fun r -> Array.iter (fun v -> f v) r) g

let iteri f g =
  Array.iteri (fun y r -> Array.iteri (fun x v -> f (x,y) v) r) g

let copy g =
  Array.copy g |> Array.map Array.copy

let count pred g =
    let c = ref 0 in
    let f x = 
        if pred x
        then incr c
        else ()
    in
    iter f g;
    !c

let collecti pred g =
  let ret = ref [] in
  let f loc v =
    if pred loc v
    then ret := (v,loc)::!ret
    else ()
  in
  iteri f g;
  !ret

let collect pred g =
  let pred' _ v = pred v in
  collecti pred' g

let rowsi g =
  let h = height g in
  let row_idxes = 0 @.. (h-1) in
  let w = width g in
  let col_idxes = 0 @.. (w-1) in
  List.map (fun y -> List.map (fun x -> ((x,y), get_space_exn g (x,y))) col_idxes) row_idxes

let rows g =
  rowsi g
  |> List.map (fun r -> List.map (fun (_, o) -> o) r)
  
let columnsi g =
  let h = height g in
  let row_idxes = 0 @.. (h-1) in
  let w = width g in
  let col_idxes = 0 @.. (w-1) in
  List.map (fun x -> List.map (fun y -> ((x,y), get_space_exn g (x,y))) row_idxes) col_idxes

let columns g =
  columnsi g
  |> List.map (fun c -> List.map (fun (_, o) -> o) c)

let to_left_of g (x,y) =
  let col_idxes = List.rev @@ 0 @.. (x-1) in
  List.map (fun curr_x -> space_and_loc_exn g (curr_x, y))  col_idxes
  
let to_right_of g (x,y) = 
  let low = x+1 in
  let high = (width g) - 1 in
  let col_idxes = low @.. high in
  List.map (fun curr_x -> space_and_loc_exn g (curr_x, y))  col_idxes

let above g (x,y) = 
  let low = 0 in
  let high = y - 1 in
  let row_idxes = List.rev @@ low @.. high in
  List.map (fun curr_y -> space_and_loc_exn g (x, curr_y)) row_idxes

let below g (x,y) =
  let low = y + 1 in
  let high = (height g) - 1 in
  let row_idxes = low @.. high in
  List.map (fun curr_y -> space_and_loc_exn g (x, curr_y)) row_idxes