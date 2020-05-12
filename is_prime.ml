(* purpose: 自然数 n を受け取り，それが素数かどうか判定する *)
(* is_prime: int -> bool *)
let is_prime n =
    if n > 2 then
        let rec loop i =
            if i * i > n then true
            else if n mod i = 0 then false
            else loop (i+1)
        in loop 2
    else false

(* test *)
let test1 = is_prime 2 = false
let test2 = is_prime 3 = true
let test3 = is_prime 11 = true
let test4 = is_prime 12 = false
let test5 = is_prime 71 = true
let test6 = is_prime 75 = false
let test7 = is_prime 859 = true
let test8 = is_prime 1009 = true
let test9 = is_prime 1088 = false
let test10 = is_prime 3761 = true
