open Position
open! Base

module Line = struct
  type t = { slope: int; b: int }

  let y_of_x t x =
    let {slope=m;b} = t in
    m * x + b

  let x_of_y t y =
    let {slope=m;b} = t in
    (y - b) / m

  let intersect l1 l2 =
    let {slope=m1;b=b1} = l1 in
    let {slope=m2;b=b2} = l2 in
    let x = (b2 - b1) / (m1 - m2) in
    let y = m1 * x + b1 in
    (x,y)

  let contains t (x,y) =
    y_of_x t x = y
end

type t = 
  | Segment of pos * pos * Line.t
  | Point of pos

let from_points (x1,y1) (x2,y2) : t =
  let open Line in
  if x1 = x2 && y1 = y2
  then
    Point (x1,y1)
  else
  let l,r = 
    if x1 < x2
    then ((x1,y1),(x2,y2))
    else ((x2,y2),(x1,y1))
  in
  let (lx, ly) = l in
  let (rx, ry) = r in
  let slope = (ry - ly) / (rx - lx) in
  let b = ly - (slope * lx) in
  let line = {slope;b} in
  Segment (l, r, line)

let from_line_and_xs l x1 x2 : t =
  let open Line in
  if x1 = x2
  then 
    Point (x1, y_of_x l x1)
  else
    let lx,rx = (min x1 x2, max x1 x2) in
    let ly = y_of_x l lx in
    let ry = y_of_x l rx in
    Segment ((lx,ly),(rx,ry), l)

let contains (s:t) (x,y) =
  match s with
  | Point (px, py) -> px = x && py = y
  | Segment ((lx,_),(ux,_),line) ->
    lx <= x && x <= ux && Line.contains line (x,y)

let trim_to_fit (t: t) lx ux ly uy : t option =
  match t with
  | Point (x,y) -> 
    if x > ux || x < lx || y > uy || y < ly
    then None
    else Some t
  | Segment ((x1,y1), (x2, y2), line) ->
  let trim_point (x,y) =
    let open Base.Option.Let_syntax in
    let%bind (x, y) =
      if x > ux
      then
        let new_y = Line.y_of_x line ux in
        if new_y < ly || new_y > uy 
        then begin
          (* Stdio.printf "returning None at x > ux: %d > %d new_y: %d \n" x ux new_y; *)
          None
        end
        else Some (ux,new_y)
      else if x < lx 
      then
        let new_y = Line.y_of_x line lx in
        if new_y < ly || new_y > uy 
        then begin
          (* Stdio.printf "returning None at x < lx: %d < %d new_y: %d\n" x lx new_y; *)
          None
        end
        else Some (lx,new_y)
      else
        Some (x,y)
    in
    let%bind (x, y) =
      if y < ly
      then
        let new_x = Line.x_of_y line ly in
        if new_x < lx || new_x > ux
        then begin
          (* Stdio.printf "returning None at y < ly: %d < %d\n" y ly; *)
          None
        end
        else Some (new_x, ly)
      else 
        if y > uy
        then
          let new_x = Line.x_of_y line uy in
          if new_x < lx || new_x > ux
          then begin
            (* Stdio.printf "returning None at y > uy: %d > %d\n" y uy; *)
            None
          end
          else Some (new_x, uy)
      else Some (x, y)
    in
    Some (x, y)
  in
  match (trim_point (x1,y1), trim_point (x2,y2)) with
  | (Some p1, Some p2) -> 
    (* Make sure the slope is the same as the original? 
       It should be, but want to make sure *)
    Some (from_points p1 p2)
  | _ -> None

let intersect (s1: t) (s2: t) = 
  match (s1,s2) with
  | Point (x1,y1), Point (x2,y2) ->
    (if x1 = x2 && y1 = y2
    then Some (x1,y1)
    else None)
  | ((Point p), ((Segment _) as s))
  | ((Segment _) as s, (Point p)) ->
    let (px,py) = p in
    if contains s (px,py) 
    then Some (px,py)
    else None
  | Segment ((lx1,_),(ux1,_),l1) , Segment ((lx2,_),(ux2,_),l2) ->
    let (ix,iy) = Line.intersect l1 l2 in
    if ix < lx1 || ix > ux1 || ix < lx2 || ix > ux2
    then None
    else Some (ix,iy)

let occlude_segment (s: t) (b1: t) (b2: t) : t list =
  match s with
  | Point _ ->
    (match (intersect s b1), (intersect s b2) with
     | None, None -> [s]
     | _ -> 
       (* If any boundary intersects point, point is occluded*)
       []
    )
  | Segment ((sx1,_),(sx2,_),line) ->
  match (intersect s b1, intersect s b2) with
  | (Some (x1,_), Some (x2,_)) ->
    (let (ix1,ix2) = (min x1 x2, max x1 x2) in
      match (sx1 < ix1, ix2 < sx2) with
      | (true,true) -> (* split into two segments *)
        [ 
          from_line_and_xs line sx1 (ix1-1);
          from_line_and_xs line (ix2+1) sx2;
        ]
      | (true,false) -> (* one segment *)
        [ from_line_and_xs line sx1 (ix1-1)]
      | (false, true) -> (* one segment *)
        [ from_line_and_xs line (ix2+1) sx2]
      | _ -> (*completely occluded *)
        []
    )
  | _ -> 
    (* boundaries don't intersect segment, nothing occluded *)
    [s]

let slope (s:t) : int =
  let open Line in
  match s with
  | Point _ -> failwith "No slope for points"
  | Segment (_,_,{slope;_}) -> slope

open Utils

let points = function
| Segment ((lx,_),(ux,_),line) ->
  let xs = lx @.. ux in
  List.map ~f:(fun x -> (x, Line.y_of_x line x)) xs
| Point p -> [p]