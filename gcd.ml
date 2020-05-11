(* purpose: n m >= 0 を受け取り, 最大公約数を返す。実装の便宜上， gcd 0 0 = 0， また 0 は単位元となっている *)
(* gcd: int -> int -> int *)
let rec gcd m n =
    if m <= n then gcd n m
    else if n = 0 then m
    else gcd n (m mod n)

(* test *)
let test1 = gcd 1 0 = 1
let test2 = gcd 2 3 = 1
let test3 = gcd 5 10 = 5
let test4 = gcd 35 100 = 5
let test5 = gcd 17 100000 = 1
let test6 = gcd 30 18 = 6
let test7 = gcd 24 36 = 12