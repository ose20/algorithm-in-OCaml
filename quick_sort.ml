(* filter: ('a -> bool) -> 'a list -> 'a list *)
let rec filter f lst = match lst with
        [] -> []
    |   first :: rest -> if f first then first :: filter f rest else filter f rest


(* quick_sort: 'a list -> 'a list *)
(* purpose: リストを受け取り，クイックソートによって昇順に整列する *)
let rec quick_sort lst = match lst with 
        [] -> []
    |   first :: rest -> quick_sort (filter (fun element -> element < first) rest) 
                        @ filter (fun element -> element = first) lst
                        @ quick_sort (filter (fun element -> element > first) rest)


(* test *)
let test1 = quick_sort [] = []
let test2 = quick_sort [1; 3; 2] = [1; 2; 3]
let test3 = quick_sort [3; 4; 5] = [3; 4; 5]
let test4 = quick_sort [3; 2; 3; 1; 2; 3] = [1; 2; 2; 3; 3; 3]
