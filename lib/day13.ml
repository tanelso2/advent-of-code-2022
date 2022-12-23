open Utils

module Packet = struct
    module T = struct
        open Base

        type t = 
        PVal of int
        | PList of t list [@@deriving sexp]
    end

    include T

    let rec show = function
    | PVal v -> Int.to_string v
    | PList l -> 
        let children = List.map show l in
        Printf.sprintf "[%s]" (String.concat " " children)

    let rec compare a b =
        (* Stdio.printf "Comparing: %s,   %s\n" (show a) (show b); *)
        match (a,b) with
        | (PVal x, PVal y) -> Int.compare x y
        | (((PVal _) as v), ((PList _) as l)) -> compare (PList [v]) l 
        | (((PList _) as l), ((PVal _) as v)) -> compare l (PList [v])
        | (PList l1, PList l2) -> 
            let open Base.Continue_or_stop in
            let f () = function
            | `Left _ -> Stop (1)
            | `Right _ -> Stop (-1)
            | `Both (t1,t2) -> 
                    let res = compare t1 t2 in
                    if res = 0
                    then Continue ()
                    else Stop res
            in
            let open Base.Sequence in
            zip_full (of_list l1) (of_list l2)
            |> fold_until ~init:() ~f:f ~finish:(fun () -> 0)
    
    let rec parse_list cs acc =
        match cs with
        | [] -> failwith "Nothing left"
        | c::rest ->
            (* Stdio.printf "parsing list with '%c'\n" c; *)
            match c with
            | ']' -> 
                (* Stdio.printf "Returning list\n"; *)
                (rest, PList acc)
            | '[' -> 
                let (rest',x) = parse_list rest [] in
                parse_list rest' (acc @ [x])
            | '0'..'9' -> 
                let (rest',x) = parse_val rest [c] in
                parse_list rest' (acc @ [x])
            | _ -> parse_list rest acc
    and parse_val cs acc =
        match cs with
        | [] -> failwith "Nothing left"
        | c::rest ->
            (* Stdio.printf "parsing val with '%c'\n" c; *)
            match c with
            | '0'..'9' -> parse_val rest (c::acc)
            | _ -> 
                let v = acc |> List.rev |> Base.String.of_char_list |> int_of_string in
                (* Stdio.printf "returning val %d\n" v; *)
                (cs, PVal v)
    and parse s =
        let cs = Base.String.to_list s
                 |> Base.List.drop_while ~f:((!=) '[')
                 |> List.tl
        in
        match parse_list cs [] with
        | (_,r) -> r
    
    let equals = ((=) 0) <<.. compare
end


let parse_pairs s =
    let groups = 
        trimmed_lines_of s
        |> ListUtils.split_on ~sep:""
    in
    let parse_pair = function
    | [p1;p2] -> Some (Packet.parse p1, Packet.parse p2)
    | _ -> None
    in
    List.filter_map parse_pair groups

let parse_lines s = 
    trimmed_lines_of s 
    |> List.filter (fun x -> String.length x > 0)
    |> List.map Packet.parse

let in_right_order p1 p2 =
    (* Stdio.printf "p1 is %s, p2 is %s\n" (Packet.show p1) (Packet.show p2); *)
    let res = Packet.compare p1 p2 in
    (* Stdio.printf "compare returned %d\n" res; *)
    res = -1

let part1 (s:string) =
    s
    |> parse_pairs
    |> List.mapi (fun i p -> i,p)
    |> List.filter (fun (_,(p1,p2)) -> in_right_order p1 p2)
    |> List.map fst
    |> List.map succ
    |> sum

let buffer_packets = 
    let open Packet in
    [
      PList [PList [PVal 2]];
      PList [PList [PVal 6]];
    ]

let lines_with_buffers s =
    let packets = parse_lines s in
    packets @ buffer_packets

let find_pos ps p =
    Base.List.findi_exn ~f:(fun _ x -> Packet.equals x p) ps
    |> fst
    |> succ

let part2 (s:string) =
    let packets = lines_with_buffers s in
    let packets = List.sort Packet.compare packets in
    List.map (find_pos packets) buffer_packets
    |> product
