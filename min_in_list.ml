(* min_in_list: 'a list -> 'a *)
(* purpose: lstを受け取り，その中の最小値を返す *)
let rec min_in_list lst = match lst with
        [] -> max_int
    |   first :: rest ->
        let rest_result = min_in_list rest
        in if first < rest_result then first
        else rest_result

(* test *)
let test1 = min_in_list [10; 3; 2; 1] = 1
let test2 = min_in_list [2] = 2
let test3 = min_in_list [] = max_int