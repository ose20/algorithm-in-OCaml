(* reverse_list: 'a list -> 'a list *)
(* purpose: リストを受け取り，反転して返す *)
let reverse_list lst =
    (* helper: 'a list -> 'a list -> 'a list *)
    (* purpose: まだ反転していないリストの右部分と，既に反転したリストの左半分を受け取り，元のリストを反転したリストを返す *)
    let rec helper lst rev_lst = match lst with
            [] -> rev_lst
        |   first :: rest -> helper rest (first :: rev_lst)
    in helper lst []


(* test *)
let test1 = reverse_list [] = []
let test2 = reverse_list [1; 2; 3; 4; 5] = [5; 4; 3; 2; 1]
let test3 = reverse_list ["one"; "two"; "three"] = ["three"; "two"; "one"]