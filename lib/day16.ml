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

type worker_status = 
    AtValve of Valve.t
    | EnRoute of int * Valve.t
    | AllDone

let enroute t dest =
    if t = 0
    then AtValve dest
    else EnRoute (t, dest)

let find_max_journies ?(max_time = 26) vs =
    let m = floydwarshall vs in
    let get_length a b =
        Map.find_exn m (a,b)
    in
    let open Valve in
    let module S = ValveSet in
    let wait_it_out time open_flow total_flow =
        let time_left = max_time - time in
        total_flow + time_left * open_flow
    in
    let rec helper (worker1, worker2) nodes_left time open_flow total_flow =
        if time >= max_time
        then total_flow
        else
            match (worker1, worker2) with
            | (AtValve v1, AtValve v2) -> 
                let (time', total_flow', open_flow') =
                    if v1.name = "AA"
                    then 
                        (* Don't open the starting valve *)
                        (time, total_flow, open_flow)
                    else
                        (* Open both valves *)
                        let time' = time + 1 in
                        let total_flow' = total_flow + open_flow in
                        let open_flow' = open_flow + v1.flow_rate + v2.flow_rate in
                        (time', total_flow', open_flow')
                    in
                (match S.cardinal nodes_left with
                | 0 -> wait_it_out time' open_flow' total_flow'
                | 1 -> (
                    let dest = S.min_elt nodes_left in
                    let eta1 = get_length v1.name dest.name in
                    let eta2 = get_length v2.name dest.name in
                    if eta1 < eta2
                    then helper (enroute eta1 dest, AllDone) S.empty time' open_flow' total_flow'
                    else helper (AllDone, enroute eta2 dest) S.empty time' open_flow' total_flow'
                ) 
                | _ -> (
                (* Iterate over nodes_left twice, assign a destination to each *)
                (* recurse *)
                let handle_worker1_dest dest1 =
                    let eta1 = get_length v1.name dest1.name in
                    let nodes' = S.remove dest1 nodes_left in
                    let handle_worker2_dest dest2 =
                        let eta2 = get_length v2.name dest2.name in
                        let nodes'' = S.remove dest2 nodes' in
                        helper (enroute eta1 dest1, enroute eta2 dest2) nodes'' time' open_flow' total_flow'
                    in
                    S.elements nodes' |> List.map handle_worker2_dest |> ListUtils.max_by_exn ~key_fn:Fun.id
                in
                S.elements nodes_left |> List.map handle_worker1_dest |> ListUtils.max_by_exn ~key_fn:Fun.id
             )
            )
        | (AtValve v, EnRoute (eta, dest)) | (EnRoute (eta, dest), AtValve v) ->
            (* Open valve *)
            let time' = time + 1 in
            let total_flow' = total_flow + open_flow in
            let open_flow' = open_flow + v.flow_rate in
            let worker2' = enroute (eta - 1) dest in
            (* Find a node to go to *)
            let handle_worker1_dest dest1 =
                let nodes' = S.remove dest1 nodes_left in
                let eta1 = get_length v.name dest1.name in
                let worker1 = enroute eta1 dest1 in
                helper (worker1, worker2') nodes' time' open_flow' total_flow'
            in
            if S.cardinal nodes_left = 0
            then 
                (* Try racing the other node to the same dest *)
                let eta1 = get_length v.name dest.name in
                let worker1' = enroute eta1 dest in
                match worker2' with
                | EnRoute (eta2', _) -> 
                    if eta1 < eta2' 
                    then helper (worker1', AllDone) nodes_left time' open_flow' total_flow'
                    else helper (AllDone, worker2') nodes_left time' open_flow' total_flow'
                | AtValve _ ->
                    helper (AllDone, worker2') nodes_left time' open_flow' total_flow'
                | AllDone ->
                    helper (AllDone, AllDone) nodes_left time' open_flow' total_flow'
            else
                (* Try visiting all other nodes *)
                S.to_seq nodes_left 
                |> Seq.map handle_worker1_dest 
                |> List.of_seq 
                |> ListUtils.max_by_exn ~key_fn:Fun.id
        | (EnRoute (eta1, dest1), EnRoute (eta2, dest2)) -> 
            let travel_time = Int.min eta1 eta2 in
            if time + travel_time > max_time
            then wait_it_out time open_flow total_flow
            else
                let eta1' = eta1 - travel_time in
                let eta2' = eta2 - travel_time in
                let worker1' = enroute eta1' dest1 in
                let worker2' = enroute eta2' dest2 in
                let time' = time + travel_time in
                let total_flow' = total_flow + (open_flow * travel_time) in
                helper (worker1', worker2') nodes_left time' open_flow total_flow'
        | (EnRoute (eta, dest), AllDone) | (AllDone, EnRoute (eta, dest)) ->
            if time + eta > max_time
            then wait_it_out time open_flow total_flow
            else
                let time' = time + eta in
                let total_flow' = total_flow + (open_flow * eta) in
                helper (AtValve dest, AllDone) nodes_left time' open_flow total_flow'
        | (AtValve v, AllDone) | (AllDone, AtValve v) ->
            (* Open valve *)
            let time' = time + 1 in
            let total_flow' = total_flow + open_flow in
            let open_flow' = open_flow + v.flow_rate in
            helper (AllDone, AllDone) nodes_left time' open_flow' total_flow'
        | (AllDone, AllDone) -> wait_it_out time open_flow total_flow
    in
    let start = find_valve vs "AA" in
    let dests = (S.of_list @@ valves_with_flow vs) in
    helper (AtValve start, AtValve start) dests 0 0 0

let find_max_journey ?(max_time = 30) vs  =
    let m = floydwarshall vs in
    let open Valve in
    let module S = ValveSet in
    let rec helper curr_node nodes_left time open_flow total_flow =
        let open_valve node =
            if node.name = "AA"
            then (time, open_flow, total_flow)
            else (time + 1, open_flow + node.flow_rate, total_flow + open_flow)
        in
        if time = max_time
        then (total_flow, nodes_left)
        else
            let (time, open_flow, total_flow) = open_valve curr_node in
            (* Remove the current node *)
            let nodes' = S.remove curr_node nodes_left in
            (* *)
            let travel_to_dest dest : int * S.t =
                let travel_time = Map.find_exn m (curr_node.name, dest.name) in
                let time_left = max_time - time in
                if travel_time > time_left
                then
                    (total_flow + (open_flow * time_left), nodes')
                else begin
                    let total_flow = total_flow + (open_flow * travel_time) in
                    let time = time + travel_time in
                    helper dest nodes' time open_flow total_flow
                end

            in
            match S.to_seq nodes' |> List.of_seq |> List.map travel_to_dest |> ListUtils.max_by ~key_fn:fst with
            | None ->
                (* There are no nodes left to travel to. Just wait here until 30 seconds *)
                let time_left = max_time - time in
                (total_flow + (open_flow * time_left), S.empty)
            | Some v -> v
    in
    let dests = (S.of_list @@ valves_with_flow vs) in
    let start = find_valve vs "AA" in
    helper start dests 0 0 0

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

(* let find_best vs = *)
(*     let m = floydwarshall vs in *)
(*     let flow_valve_names = valves_with_flow vs  *)
(*         |> List.map Valve.get_name *)
(*     in *)
(*     let best_result = ref Int.min_int in *)
(*     let check_perm perm = *)
(*         let score = score_result m vs perm in *)
(*         if score > !best_result *)
(*         then best_result := score *)
(*         else () *)
(*     in *)
(*     let perms = ListUtils.permutations flow_valve_names in *)
(*     perms check_perm; *)
(*     !best_result *)

let part1 (s:string) =
    let vs = parse s in
    let (v,_) = find_max_journey vs in
    v

let part2 (s:string) =
    let vs = parse s in
    find_max_journies vs
