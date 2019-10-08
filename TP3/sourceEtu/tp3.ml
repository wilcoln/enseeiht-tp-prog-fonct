let rec code_gray n = 
    if n = 0 then  [[]]
    else 
        let l =  code_gray (n-1) 
            in (List.map (fun x -> 0::x) l)@(List.map (fun x -> 1::x) l)
 

 let rec combinaisons k l = 
 let n = List.length l 
 in  
    if n < k || k = 0 then [[]]
    else if n = k then [l]
    else (List.map (fun x -> (List.hd l)::x) (combinaisons (k-1) (List.tl l )))@(combinaisons k (List.tl l))

let rec insertions_partout e l = 
    match l with 
    |[] -> [[e]]
    |t::q -> [e::l]@(List.map (fun x -> t::x ) (insertions_partout e q))


let rec permutations l = 
    match l with 
    | [] -> [[]]
    | [t] -> [[t]]
    | t::q -> List.flatten (List.map (insertions_partout t) (permutations q))
     



