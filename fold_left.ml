(* fold_left: ('a -> 'b -> 'a) -> 'b list -> 'a *)
(* purpose: 2引数関数 f と init と lst を受け取り， lst をの要素に左から f を施しこむ *)
(* example: fold_left f init [1; 2; 3] = f (f (f init 1) 2) 3 *)
let rec fold_left f init lst = match lst with
        [] -> init
    |   first :: rest -> fold_left f (f init first) rest

(* test *)
let test1 = fold_left ( * ) 1 [1; 2; 3] = 6
let test2 = fold_left (fun lst a -> a :: lst) [] [1; 2; 3] = [3; 2; 1]