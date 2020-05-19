(* 二分木の定義 *)
type tree_t =
    Empty                           (* 空の木 *)
  | Leaf of int                     (* 葉 *)
  | Node of tree_t * int * tree_t   (* 節 *)


(* 二分木の例 *)
let tree1 = Leaf (1)
let tree2 = Leaf (5)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Leaf (7)
let tree5 = Node (tree3, 6, tree4)


(* 二分木を使ったいろいろな関数 *)

(* purpose: tree_t 上の整数をすべて足して返す *)
(* sum_tree: tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (t1, n, t2) -> (sum_tree t1) + n + (sum_tree t2)

(* test *)
let test1 = sum_tree tree1
let test2 = sum_tree tree2
let test3 = sum_tree tree3
let test4 = sum_tree tree4
let test5 = sum_tree tree5


(* purpose: tree_t のすべての要素に関数 f を施す *)
(* tree_map: (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2)

(* test *)
let test6 = tree_map (fun x -> 2*x) tree5


(* purpose: 木の長さを計算する *)
(* tree_length: tree_t -> int *)
let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (n) -> 1
  | Node (t1, n, t2) -> tree_length t1 + 1 + tree_length t2

(* test *)
let test7 = tree_length tree1
let test8 = tree_length Empty
let test9 = tree_length tree5


(* purpose: 木の深さを計算する *)
(* tree_depth: tree_t -> int *)
let rec tree_depth tree = match tree with
    Empty -> 0
  | Leaf (n) -> 0
  | Node (t1, n, t2) -> 1 + max (tree_depth t1) (tree_depth t2)

(* test *)
let test10 = tree_depth tree1
let test11 = tree_depth tree2
let test12 = tree_depth tree3
let test13 = tree_depth tree4
let test14 = tree_depth tree5


(* purpose: tree_t を先行順巡回で探索して要素をリストにして返す *)
(* preorder_tree_list: tree_t -> int list *)
let rec preorder_tree_list tree = match tree with
    Empty -> []
  | Leaf (n) -> [n]
  | Node (t1, n, t2) -> preorder_tree_list t1 @ [n] @ preorder_tree_list t2

(* test *)
let test15 = preorder_tree_list tree5


(* purpose: tree_t を中間順巡回で探索して要素をリストにして返す *)
(* inorder_tree_list: tree_t -> int list *)
let rec inorder_tree_list tree = match tree with
    Empty -> []
  | Leaf (n) -> [n]
  | Node (t1, n, t2) -> [n] @ inorder_tree_list t1 @ inorder_tree_list t2

(* test *)
let test16 = inorder_tree_list tree5


(* purpose: tree_t を後行順巡回で探索して要素をリストにして返す *)
let rec postorder_tree_list tree = match tree with
    Empty -> []
  | Leaf (n) -> [n]
  | Node (t1, n, t2) -> postorder_tree_list t1 @ postorder_tree_list t2 @ [n]

(* test *)
let test17 = postorder_tree_list tree5