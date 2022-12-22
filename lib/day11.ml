open! Utils
open Utils.RegexUtils

type monkey = {
    num: int;
    mutable holding: int list;
    op: int -> int;
    test: int -> bool;
    true_target: int;
    false_target: int;
    mutable inspections: int;
}

type expr = 
    | Const of int
    | Old
    | Op of op * expr * expr
and op =
    | Plus
    | Mult

let num_r = Re.Posix.compile_pat {|Monkey ([0-9]+):|}
let items_r = Re.Posix.compile_pat {|Starting items: ([0-9]+(, [0-9]+)*)|}
let op_r = Re.Posix.compile_pat {|Operation: new = ([^/\r\n]+)|}
let test_r = Re.Posix.compile_pat {|Test: divisible by ([0-9]+)|}
let true_target_r = Re.Posix.compile_pat {|If true: throw to monkey ([0-9]+)|}
let false_target_r = Re.Posix.compile_pat {|If false: throw to monkey ([0-9]+)|}

let make_test d = (fun x -> x mod d = 0)

let make_items s =
    String.split_on_char ',' s
    |> List.map String.trim
    |> List.map int_of_string

let parse_op = function
| "+" -> Plus
| "*" -> Mult
| _ -> failwith "Could not parse op"

let parse_val = function
| "old" -> Old
| s -> Const (int_of_string s)

let make_op s : expr =
    match s |> String.trim |> String.split_on_char ' ' with
    | [e1;o;e2] -> Op (parse_op o, parse_val e1, parse_val e2)
    | _ -> failwith "Op expression contained more than 3 words"

let op_to_fn = function
| Plus -> (+)
| Mult -> ( * )

let rec eval_op_with v = function
| Old -> v
| Const x -> x
| Op (o,e1,e2) -> (op_to_fn o) (eval_op_with v e1) (eval_op_with v e2)

let make_op_fn op = fun v -> eval_op_with v op

let test_fn (s:string) =
    let num = get_first_group num_r s |> int_of_string in
    let items = get_first_group items_r s |> make_items in
    let op = get_first_group op_r s |> make_op |> make_op_fn in
    let divisible_by = get_first_group test_r s |> int_of_string in
    let test = make_test divisible_by in
    let true_target = get_first_group true_target_r s |> int_of_string in
    let false_target = get_first_group false_target_r s |> int_of_string in
    (num, items, op, divisible_by, true_target, false_target, test)

let parse_monkey (s:string) : monkey = 
    let num = get_first_group num_r s |> int_of_string in
    let holding = get_first_group items_r s |> make_items in
    let op = get_first_group op_r s |> make_op |> make_op_fn in
    let divisible_by = get_first_group test_r s |> int_of_string in
    let test = make_test divisible_by in
    let true_target = get_first_group true_target_r s |> int_of_string in
    let false_target = get_first_group false_target_r s |> int_of_string in
    {num;
     holding;
     op;
     test;
     true_target;
     false_target;
     inspections = 0}

let parse_monkeys s = 
    String.split_on_char '\n' s
    |> split_on ~sep:""
    |> List.map (String.concat "\n")
    |> List.filter (fun x -> String.length x > 0)
    |> List.map parse_monkey

let throw_to ms target v = 
    let x = ms.(target) in
    let vs = x.holding in
    x.holding <- vs @ [v]

let process_ball ms curr v =
    let v' = curr.op v in
    let v'' = v' / 3 in
    let target = if curr.test v''
                 then curr.true_target
                 else curr.false_target
    in
    throw_to ms target v''

let process_turn ms thrower =
    List.iter (process_ball ms thrower) thrower.holding;
    let num_processed = List.length thrower.holding in
    thrower.holding <- [];
    thrower.inspections <- thrower.inspections + num_processed

let process_round ms _ =
    List.iter (fun i -> process_turn ms ms.(i)) (0 @.. (Array.length ms - 1))

let process_rounds num_rounds ms =
    let ma = Array.of_list ms in
    let round_nums = 1 @.. num_rounds in
    List.iter (process_round ma) round_nums;
    ma

let monkey_business ms =
    ms
    |> Array.map (fun {inspections;_} -> inspections)
    |> Array.to_list
    |> List.sort (Int.neg <<.. Int.compare)
    |> Fun.flip Base.List.take 2
    |> product

let part1 (s:string) =
    parse_monkeys s
    |> process_rounds 20
    |> monkey_business

let process_ball' ms curr v =
    let v' = curr.op v in
    let target = if curr.test v'
                 then curr.true_target
                 else curr.false_target
    in
    throw_to ms target v'

let process_turn' ms thrower =
    List.iter (process_ball' ms thrower) thrower.holding;
    let num_processed = List.length thrower.holding in
    thrower.holding <- [];
    thrower.inspections <- thrower.inspections + num_processed

let process_round' ms _ =
    List.iter (fun i -> process_turn' ms ms.(i)) (0 @.. (Array.length ms - 1))

let process_rounds' num_rounds ms =
    let ma = Array.of_list ms in
    let round_nums = 1 @.. num_rounds in
    List.iter (process_round' ma) round_nums;
    ma

let part2 (s:string) =
    parse_monkeys s
    |> process_rounds' 10000 
    |> monkey_business
