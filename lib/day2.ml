open Utils

type rps = Rock | Paper | Scissors

type game_result = Win | Lose | Draw

let game_result opponent me =
    match (opponent,me) with
    | (Rock,Paper) -> Win
    | (Rock, Scissors) -> Lose
    | (Paper, Rock) -> Lose
    | (Paper, Scissors) -> Win
    | (Scissors, Paper) -> Lose
    | (Scissors, Rock) -> Win
    | _ -> Draw

let parse_opponent = function
| "A" -> Rock
| "B" -> Paper
| "C" -> Scissors
| _ -> failwith "failed to parse opponent"

let parse_me = function
| "X" -> Rock
| "Y" -> Paper
| "Z" -> Scissors
| _ -> failwith "failed to parse me"

let parse_expected_result = function
| "X" -> Lose
| "Y" -> Draw
| "Z" -> Win
| _ -> failwith "failed to parse expected result"

let parse_matchup s =
    let f = function
        | [x;y] -> (parse_opponent x, parse_me y)
        | _ -> failwith "Failed to parse matchup"
    in
    String.split_on_char ' ' s
    |> Fun.flip Base.List.take 2
    |> f

let parse_matchup_part2 s =
    let f = function
        | [x;y] -> (parse_opponent x, parse_expected_result y)
        | _ -> failwith "Failed to parse matchup"
    in
    String.split_on_char ' ' s
    |> Fun.flip Base.List.take 2
    |> f

let parse s =
    trimmed_lines_of s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map parse_matchup

let parse_part2 s =
    trimmed_lines_of s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map parse_matchup_part2

let score_result = function
| Win -> 6
| Draw -> 3
| Lose -> 0

let rps_score = function
| Rock -> 1
| Paper -> 2
| Scissors -> 3

let find_throw_for_result (op, expected) =
    match expected with
    | Draw -> op
    | Win -> (match op with
              | Rock -> Paper
              | Paper -> Scissors
              | Scissors -> Rock
             )
    | Lose -> match op with
              | Rock -> Scissors
              | Scissors -> Paper
              | Paper -> Rock

let calculate_score_for_round (op,me) =
    let res = game_result op me in
    score_result res + rps_score me

let part1 (s:string) =
    let rounds = parse s in
    List.map calculate_score_for_round rounds
    |> sum

let part2 (s:string) =
    let rounds = parse_part2 s in
    let f (op,expected) =
        (op, find_throw_for_result (op,expected))
    in
    List.map f rounds
    |> List.map calculate_score_for_round
    |> sum
