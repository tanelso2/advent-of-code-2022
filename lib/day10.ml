open Utils

type cmd = 
    Noop 
    | Addx of int

let parse_cmd l =
    if l = "noop"
    then Noop
    else if String.starts_with ~prefix:"addx" l
    then begin
        match String.split_on_char ' ' l with
        | [_;s] -> Addx (int_of_string s)
        | _ -> failwith "parsing an addx line failed"
    end
    else failwith "parse_cmd failed"

let parse s =
    trimmed_lines_of s
    |> List.filter (fun l -> String.length l > 0)
    |> List.map parse_cmd

type state = {
    x_val : int;
    time : int
}

let init_state = {x_val = 1; time = 1}

let handle_cmd {x_val;time} = function
| Noop -> {x_val; time = time + 1}
| Addx v -> {x_val = x_val + v;
             time = time + 2}

let handle_cmds cmds = 
    let rest = Base.List.folding_map 
                          ~f:(fun acc v -> 
                                let r = handle_cmd acc v in 
                                (r,r))
                          ~init:init_state
                          cmds
    in
    init_state::rest

let find_state_at states search_time =
    Base.List.take_while states ~f:(fun {time;_} -> time <= search_time)
    |> Base.List.last_exn

let important_times = [20; 60; 100; 140; 180; 220]

let find_important_states states =
    List.map (find_state_at states) important_times

let signal_strength s =
    parse s
    |> handle_cmds
    |> find_important_states
    |> List.map (fun {x_val;_} -> x_val)
    |> Base.List.zip_exn important_times
    |> Base.List.map ~f:(fun (t,v) -> t * v)
    |> sum

let part1 (s:string) =
    signal_strength s

let all_pixels = 1 @.. 240

let state_at_each_cycle states =
    List.map (fun i -> 
                let {x_val;_} = find_state_at states i in
                {x_val; time = i}
             ) all_pixels

let should_draw {x_val; time} =
    let col = ((time - 1) mod 40) in
    x_val - 1 = col || x_val = col || x_val + 1 = col

let draw_cycles states =
    state_at_each_cycle states 
    |> List.map should_draw
    |> Base.List.chunks_of ~length:40
    |> List.map (fun l -> List.map (fun c -> if c then '#' else '.') l)
    |> List.map Base.String.of_char_list
    |> String.concat "\n"

let part2 (s:string) =
    s
    |> parse
    |> handle_cmds
    |> draw_cycles
