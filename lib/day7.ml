open Utils.StringUtils
open Utils.ListUtils

type command = {
    input: string;
    output: string list;
}

module Dir = struct
    type dir_entry = 
        Dir of {name: string; mutable entries: dir_entry list}
        | File of string * int

    let get_name = function
    | Dir {name;_} -> name
    | File (name,_) -> name

    let mkdir name = Dir {name; entries = []}

    let touch name size = File (name,size)

    let get_file_size = function
    | Dir _ -> failwith "can't get file size of dir"
    | File (_, size) -> size

    let rec total_size = function
    | File (_, size) -> size
    | Dir {entries;_} ->
        List.map total_size entries |> sum

    let get_children = function
    | File _ -> failwith "files have no children"
    | Dir {entries;_} -> entries

    let get_child parent child_name =
        match parent with
        | File _ -> None 
        | Dir {entries;_} ->  
            List.find_opt (fun c -> get_name c = child_name) entries

    let add_child parent child =
        match parent with
        | File _ -> failwith "can't add a child to a file"
        | Dir ({entries;_} as d) ->
            let child_name = get_name child in
            match get_child parent child_name with
            | Some _ -> failwith "child already exists"
            | None -> d.entries <- child::entries

    let handle_ls curr_dir output =
        match curr_dir with
        | File _ -> failwith "Can not ls on a file"
        | Dir _ ->
            let f l =
                match String.split_on_char ' ' l with
                | [a;b] -> (
                    if a = "dir"
                    then add_child curr_dir (mkdir b)
                    else 
                        let size = int_of_string a in
                        let new_file = touch b size in
                        add_child curr_dir new_file
                )
                | _ -> failwith "Couldn't parse ls output"
            in
            List.iter f output

    let construct_dir_tree (cmds: command list) : dir_entry =
        let root_dir = mkdir "/" in
        let curr_dir = ref root_dir in
        let history_stack = Stack.create () in
        let handle_cd = function
            | "/" -> begin
                Stack.clear history_stack;
                curr_dir := root_dir
            end
            | ".." -> begin
                curr_dir := (Stack.pop history_stack)
            end
            | dir -> begin
                Stack.push !curr_dir history_stack;
                match get_child !curr_dir dir with
                | Some d -> curr_dir := d
                | None -> 
                    let new_dir = mkdir dir in
                    add_child !curr_dir new_dir;
                    curr_dir := new_dir
            end
        in
        let handle_cmd {input;output} =
            match String.split_on_char ' ' input with 
            | [] -> failwith "empty input to cmd"
            | cmd::args -> 
                match cmd with
                | "ls" -> handle_ls !curr_dir output
                | "cd" ->
                    let dest = List.hd args in
                    handle_cd dest
                | _ -> failwith "unknown command"
        in
        List.iter handle_cmd cmds;
        root_dir
    
    let iter_size ~f:f d = 
        let rec helper = function
            | File (_,size) as file -> f file size; size
            | Dir {entries;_} as dir ->
                let total_size = List.map helper entries |> sum in
                f dir total_size;
                total_size
        in
        let _ = helper d in
        ()
end

include Dir

let parse_input_output s =
    match trimmed_lines_of s with
    | input::output -> 
        let output = List.filter (fun x -> String.length x > 0) output in
        {input;output}
    | _ -> failwith "parse_input_output failed"

let parse s =
    String.split_on_char '$' s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map parse_input_output

let part1 (s:string) =
    let root_dir = parse s |> Dir.construct_dir_tree in
    let res = ref [] in
    let collect x size =
        match x with
        | File _ -> ()
        | Dir {name; _} -> 
            if size <= 100000
            then res := (name,size)::!res
            else ()
    in
    Dir.iter_size ~f:collect root_dir;
    !res
    |> List.map (fun (_,s) -> s)
    |> sum

let total_space = 70000000

let needed_space = 30000000

let need_to_delete_space dir =
    let used_space = total_size dir in
    let free_space = total_space - used_space in
    needed_space - free_space

let part2 (s:string) =
    let root_dir = parse s |> Dir.construct_dir_tree in
    let target_space = need_to_delete_space root_dir in
    let smallest_valid = ref Int.max_int in
    let find_smallest_valid d size =
        match d with
        | File _ -> ()
        | Dir _ -> 
            if size > target_space
            then begin
                    if size < !smallest_valid
                    then smallest_valid := size
                    else ()
                end
            else ()
    in
    Dir.iter_size ~f:find_smallest_valid root_dir;
    !smallest_valid
