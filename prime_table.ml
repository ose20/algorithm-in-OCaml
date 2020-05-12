(* 補助関数 *)
(* purpose: 自然数 n を受け取り， [2; 3; .. ; n] となるようにリストを返す。 *)
(* two_to_n: int -> int list *)
let two_to_n n =
    let rec loop i = if i <= n then i :: loop (i+1) else []
    in loop 2

(* filter: ('a -> bool) -> 'a list -> 'a list *)
let rec filter f lst = match lst with
        [] -> []
    |   first :: rest -> if f first then first :: filter f rest else filter f rest


(* 本命 *)
(* purpose: 自然数 n を受け取り， n 以下の素数のリストを返す。実装方法はエラトステネスの篩による。 *)
(* prime_table: int -> int list *)
let prime_table n = 
    let rec seive lst = match lst with
            [] -> []
        |   first :: rest -> first :: seive (filter (fun element -> element mod first <> 0) rest)
    in seive (two_to_n n)


let test1 = prime_table 30 = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]