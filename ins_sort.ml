(* ins_sort: 'a list -> 'a list *)
(* purpose: lstを受け取るので，その値を昇順にソートする。アルゴリズムは挿入法に従う。 *)
let rec ins_sort lst = match lst with
        [] -> []
    |   first :: rest ->
        (* insert: 'a -> 'a list -> 'a list *)
        (* purpose: 要素nと既に昇順に整列されたlstを受け取り，昇順を保つようにnを挿入する。同じ要素があるなら先頭に入れる *)
        let rec insert n lst = match lst with
                [] -> n :: []
            |   first :: rest -> if n <= first then n :: lst else first :: insert n rest 
        in insert first (ins_sort rest)

(* test *)
let test1 = ins_sort [] = []
let test2 = ins_sort [3; 4; 1] = [1; 3; 4]
let test3 = ins_sort [5; 2; 11; 100] = [2; 5; 11; 100]
let test4 = ins_sort [1; 2; 3; 4] = [1; 2; 3; 4]
let test5 = ins_sort [3; 3; 1; 2; 2; 3] = [1; 2; 2; 3; 3; 3]
