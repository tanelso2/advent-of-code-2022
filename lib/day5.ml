open Utils

type instruction = {
    amount: int;
    src: int;
    dest: int;
}

let parse_instructions ls =
    (* List.iter (print_endline) ls; *)
    let r = Re.Posix.compile_pat {|move ([0-9]+) from ([0-9]+) to ([0-9]+)|} 
    in
    let f x =
        let res = Re.exec r x in
        let get_int = int_of_string << Re.Group.get res in
        {
         amount = get_int 1;
         src = get_int 2;
         dest = get_int 3
        }
    in
    List.filter (fun x -> String.length x > 0) ls
    |> List.map f


let parse_box_line s =
    let ret = ref [] in
    let f i x =
        if x >= 'A' && x <='Z'
        then
            let col = i / 4 + 1 in
            ret := (col,x)::!ret
        else ()
    in
    String.iteri f s;
    !ret

let parse_boxes s =
    (* List.iter print_endline s; *)
    let s = List.filter (fun x -> String.length x > 0 && String.exists ((=) '[') x) s in
    let box_assignments = List.rev_map parse_box_line s |> List.concat in
    let max_val = List.fold_left (fun acc (i,_) -> max i acc) 0 box_assignments in
    let res = Array.init max_val (fun _ -> Stack.create ()) in
    let add_to_stack (i,x) =
        (* Stdio.printf "adding %c to %d\n" x i; *)
        Stack.push x (res.(i - 1))
    in
    List.iter add_to_stack box_assignments;
    res

let parse s =
    let (boxes, instructions) = 
        lines_of s
        |> Base.List.split_while ~f:(fun x -> Bool.not @@ String.contains x 'm')
    in
    (parse_boxes boxes, parse_instructions instructions)

let do_instruction stacks ?(preserve_order=false) {amount;src;dest} =
    let src_stack = stacks.(src - 1) in
    let dest_stack = stacks.(dest - 1) in
    let taken = Stack.pop_n src_stack amount in
    Stack.add_seq dest_stack (taken |> (if preserve_order then Fun.id else List.rev) |> List.to_seq)

let tops_of_stacks stacks =
    Array.map Stack.top stacks

let part1 (s:string) =
    let (boxes, instructions) = parse s in
    List.iter (do_instruction boxes) instructions;
    let tops = tops_of_stacks boxes in
    (tops |> Array.to_seq |> String.of_seq)


let part2 (s:string) =
    let (boxes, instructions) = parse s in
    List.iter (do_instruction boxes ~preserve_order:true) instructions;
    let tops = tops_of_stacks boxes in
    (tops |> Array.to_seq |> String.of_seq)
