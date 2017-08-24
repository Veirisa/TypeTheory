type peano = Z | S of peano;; 
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = match x with
	0 -> Z
	| x -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
	Z -> 0
	| S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = match y with
	Z -> x
	| S yy -> add (inc x) yy;; 
	
let rec sub x y = match (x, y) with
	(Z, _) -> Z
	| (x, Z) -> x
	| ((S xx), (S yy)) -> sub xx yy;;
	
let rec mul x y = match (x, y) with
	(Z, _) -> Z
	| (_, Z) -> Z
	| (x, (S yy)) -> add x (mul x yy);;
	
let rec div x y = match (x, y, sub (inc x) y) with
	(_, Z, _) -> failwith "div by zero"
	| (Z, _, _) -> Z
	| (_, _, Z) -> Z 
	| (_, y, (S ss)) -> inc (div ss y);;

let rec power x y = match (x, y) with
	(_, Z) -> S Z (* My calculator shows: (Z, Z) -> 1 *)
	| (Z, _) -> Z
	| (x, (S yy)) -> mul x (power x yy);;

let rec print_list = function
	[] -> ()
	| h::t -> print_int h ; print_string " " ; print_list t;;

let rec do_rev x rev_l = match x with
	[] -> rev_l
	| h::t -> do_rev (t) (h::rev_l)
                     
let rev x = do_rev x [];;

let rec merge x y ans = match (x, y) with
	([], []) -> rev ans
	| ([], y_h::y_t) -> merge [] (y_t) (y_h::ans)
	| (x_h::x_t, []) -> merge (x_t) [] (x_h::ans)
	| (x_h::x_t, y_h::y_t) -> if (x_h < y_h) then (merge (x_t) (y_h::y_t) (x_h::ans)) else (merge (x_h::x_t) (y_t) (y_h::ans));;

let rec split l buf1 buf2 = match l with
	[] -> ((buf1), (buf2))
	| h::t -> split t (h::buf2) (buf1);; 

let rec merge_parse (x, y) = match (x, y) with
	([], y) -> y
	| (x, []) -> x
	| (x, y) -> merge (merge_parse (split (x) [] [])) (merge_parse (split (y) [] [])) [];;	

let merge_sort x = merge_parse (split (x) [] []);; 
                     
let rec string_of_lambda x = match x with
	Var s -> s
	| Abs (s, l) -> "\\" ^ s ^ "." ^ (string_of_lambda l)
	| App (l1, l2) -> (string_of_lambda l1) ^ " " ^ (string_of_lambda l2);;

let lambda_of_string x = failwith "Not implemented";;
