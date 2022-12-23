open Utils

type 'a t = 'a array array

type loc = int * int

let empty_grid w h init =
  Array.make_matrix h w init
and width grid =
  Array.get grid 0 |> Array.length
and height grid =
  Array.length grid


let parsei s (f: loc -> char -> 'a) : 'a t =
  let s = String.trim s in
  let lines = lines_of s |> List.map String.trim in
  let array_of_string s = s |> String.to_seq |> Array.of_seq in
  let g = Array.of_list lines 
          |> Array.mapi (fun y s -> 
              let chars = array_of_string s in 
              Array.mapi (fun x c -> (f (x,y) c)) chars) in
  g

let parse s (f: char -> 'a) =
  parsei s (fun _ c -> f c)

let to_string g (f: 'a -> char) : string =
  let show_row r = Array.map f r 
                   |> Array.to_seq
                   |> String.of_seq
  in
  String.concat "\n" (g |> Array.to_list |> List.map show_row)

let upper_right grid = (width grid - 1, 0)

let lower_left grid = (0, height grid - 1)

let lower_right grid = (width grid - 1, height grid - 1)

let get_space grid (x,y) =
  let w = width grid in
  let h = height grid in
  if x < 0 || x >= w || y < 0 || y >= h
  then None
  else Some grid.(y).(x)

let set_space grid v (x,y) =
  grid.(y).(x) <- v

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

let line_from g start finish =
  let sv = get_space_exn g start in
  if start = finish
  then [start,sv]
  else
  let fv = get_space_exn g finish in
  let get_line ~iter_to_finish ~(idx: loc -> int) =
    let innards = iter_to_finish g start
                  |> Base.List.take_while ~f:(fun (loc, _) -> idx loc != idx finish)
    in
    [start,sv] @ innards @ [finish,fv]
  in
  let open Direction in
  match start @->? finish with
  | `Same -> failwith "start and finish are the same, we already checked for that"
  | `Diag _ -> failwith "Cannot handle diagonal lines"
  | `Card d ->
    let iter = match d with
               | Up -> above
               | Down -> below
               | Right -> to_right_of
               | Left -> to_left_of
    in
    let idx = match d with
              | Up | Down -> snd
              | Left | Right -> fst
    in
    get_line ~iter_to_finish:iter ~idx:idx

let get_cardinal_neighbors g (x,y) =
  let open Direction in
  List.map (Direction.move (x,y)) [Up;Down;Left;Right]
  |> List.filter_map (fun loc -> get_space g loc |> Option.map (fun v -> (loc,v)))