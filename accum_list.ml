(* accum_list: int list -> int list *)
(* purpose: 整数のリストを受け取り，その累積和，すなわち先頭までの要素の和のリストを返す *)
let accum_list lst =
    (* helper: int list -> int -> int list *)
    (* purpose: accum_listと同じ。第二引数のacuumは今までの累積和 *)
    let rec helper lst accum = match lst with
            [] -> []
        |   first :: rest -> (first + accum) :: helper rest (first + accum)
    in helper lst 0


(* test *)
let test1 = accum_list [] = []
let test2 = accum_list [1; 2; 3; 4] = [1; 3; 6; 10]
let test3 = accum_list [2; 4; 0; 2; 0; 5] = [2; 6; 6; 8; 8; 13]