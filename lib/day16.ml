open Utils

module Valve = struct
    type t = {
        name: string;
        flow_rate: int;
        tunnels: string list;
    }

    let get_name x = x.name

    let re = Re.Posix.compile_pat 
      {|Valve ([A-Z]+) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z]+(, [A-Z]+)*)|}

    let parse s =
        let res = Re.exec re s in
        let get i = Re.Group.get res i in
        let name = get 1 in
        let flow_rate = get 2 |> int_of_string in
        let tunnels = 
            get 3 
            |> String.split_on_char ',' 
            |> List.map String.trim
            |> List.filter (fun x -> String.length x > 0)
        in
        {name;flow_rate;tunnels}

    let compare v1 v2 =
        String.compare v1.name v2.name
end

module ValveSet = Set.Make(Valve)

let parse s =
    trimmed_lines_of s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map Valve.parse

open Sets

let all_valves_with_flow_open vs open_valves =
    let open Valve in
    vs
    |> List.filter (fun {flow_rate;_} -> flow_rate > 0)
    |> List.length
    |> (=) (StringSet.cardinal open_valves)

let find_valve vs name =
    let open Valve in
    List.find (fun v -> v.name = name) vs

let valves_with_flow vs =
    let open Valve in
    vs
    |> List.filter (fun {flow_rate;_} -> flow_rate > 0)

open Maps

let floydwarshall vs =
    let open Valve in
    let dist: int StringPairMap.t ref = ref (StringPairMap.empty ()) in
    let get_val (i,j) = 
        Map.find !dist (i,j) 
    in
    let set_val (i,j) v =
        let m' = Map.set !dist ~key:(i,j) ~data:v in
        dist := m'
    in
    (* Set dist[u][v] to 1 if path between u -> v *)
    let mark_paths (v: Valve.t) : unit =
        let {name;tunnels;_} = v in
        List.iter (fun x -> set_val (name,x) 1) tunnels
    in
    List.iter mark_paths vs;
    (* Set dist[v][v] to 0 *)
    List.iter (fun {name;_} -> set_val (name,name) 0) vs;
    (* 
    for k from 1 to |V|
        for i from 1 to |V|
            for j from 1 to |V|
                if dist[i][j] > dist[i][k] + dist[k][j] 
                    dist[i][j] ← dist[i][k] + dist[k][j]
                end if *)
    let names = List.map (fun {name;_} -> name) vs in
    List.iter (fun k -> 
        List.iter (fun i ->
            List.iter (fun j -> 
                let curr = get_val (i,j) in
                let i_to_k = get_val (i,k) in
                let k_to_j = get_val (k,j) in
                    match (curr, i_to_k, k_to_j) with
                    | (Some ij, Some ik, Some kj) -> 
                        if ij > ik + kj
                        then set_val (i,j) (ik + kj)
                        else ()
                    | (None, Some ik, Some kj) ->
                        set_val (i,j) (ik + kj)
                    | _ -> ()
                ) names            
            ) names 
        ) names;
    !dist

let find_max_journey vs =
    let m = floydwarshall vs in
    let open Valve in
    let module S = ValveSet in
    let rec helper curr_node nodes_left time open_flow total_flow =
        let open_valve node =
            if node.name = "AA"
            then (time, open_flow, total_flow)
            else (time + 1, open_flow + node.flow_rate, total_flow + open_flow)
        in
        if time = 30
        then total_flow
        else
            let (time, open_flow, total_flow) = open_valve curr_node in
            (* Remove the current node *)
            let nodes' = S.remove curr_node nodes_left in
            (* *)
            let travel_to_dest dest : int =
                let travel_time = Map.find_exn m (curr_node.name, dest.name) in
                let time_left = 30 - time in
                if travel_time > time_left
                then
                    total_flow + (open_flow * time_left)
                else begin
                    let total_flow = total_flow + (open_flow * travel_time) in
                    let time = time + travel_time in
                    helper dest nodes' time open_flow total_flow
                end

            in
            S.to_seq nodes' |> List.of_seq |> List.map travel_to_dest |> ListUtils.max_by_exn ~key_fn:Fun.id
    in
    helper (find_valve vs "AA") (S.of_list @@ valves_with_flow vs) 0 0 0

let score_result m valves order =
    let open Valve in
    let curr_node = ref "AA" in
    let curr_time = ref 0 in
    let open_flow = ref 0 in
    let total = ref 0 in
    Array.iter (fun name -> 
        let {flow_rate;_} = find_valve valves name in
        (* Travel to this node *)
        let dist_traveled = Map.find_exn m (!curr_node,name) in
        if !curr_time + dist_traveled > 30
        then
            () (* Drop out of loop, we can't go any higher *)
        else
            curr_time := !curr_time + dist_traveled;
            total := !total + (!open_flow * dist_traveled);
            (* Set this node as current *)
            curr_node := name;
            (* Open this valve *)
            curr_time := !curr_time + 1;
            total := !total + !open_flow;
            open_flow := !open_flow + flow_rate;
    ) order;
    let time_left = 30 - !curr_time in
    if time_left > 0
    then (
        total := !total + (!open_flow * time_left)
    )
    else ();
    !total
    (* At each node in order - 
      for o in order:
       (* Update for distance traveled *)
       dist_traveled = dist[curr_node][o]
       curr_time += dist_traveled
       total += open_flow * dist_traveled
       (* Open the valve here *)
       open_flow += o.flow_rate
       curr_time += 1
       curr_node := o
       *)

let find_best vs =
    let m = floydwarshall vs in
    let flow_valve_names = valves_with_flow vs 
        |> List.map Valve.get_name
    in
    let best_result = ref Int.min_int in
    let check_perm perm =
        let score = score_result m vs perm in
        if score > !best_result
        then best_result := score
        else ()
    in
    let perms = ListUtils.permutations' flow_valve_names in
    perms check_perm;
    !best_result

let part1 (s:string) =
    let vs = parse s in
    find_max_journey vs

let part2 (s:string) =
    let _ = s in
    failwith "NOIMPL"
