(* 補助関数 *)
(* add_to_each: 'a -> 'a list list -> 'a list list *)
(* purpose: 'a 型の値と 'a list list 型のリストを受け取り，リストのそれぞれの要素のリストの先頭に値を追加する *)
let rec add_to_each element lst = match lst with
        [] -> []
    |   first :: rest -> (element :: first) :: add_to_each element rest

(*
    ↑最後の式は () を付けないといけない
    list の定義は
    1. 空リストはリスト
    2. first が 'a 型で， rest が 'a list 型 なら first :: rest も　'a list 型というリスト
    let hoge f lst = f :: lst が 'a -> 'a list -> 'a list 
 *)


(* 本命 *)
(* prefix_list: 'a list -> 'a list list *)
(* purpose: リストを受け取るのでその接頭辞リストを返す *)
let rec prefix_list lst = match lst with
        [] -> []
    |   first :: rest -> (first :: []) :: add_to_each first (prefix_list rest)

(* test *)
let test1 = prefix_list [] = []
let test2 = prefix_list [1] = [[1]]
let test3 = prefix_list [1; 2] = [[1]; [1; 2]]
let test4 = prefix_list [1; 2; 3; 4] = [[1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]]
