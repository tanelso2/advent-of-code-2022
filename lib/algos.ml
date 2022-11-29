let rec powerset l : 'a list Seq.t =
  match l with
  | [] -> Seq.return []
  | x :: xs -> let l = powerset xs in 
                  Seq.append l (Seq.map (fun y -> x :: y) l)
