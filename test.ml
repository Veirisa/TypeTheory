open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;
open Hw2_inference;;

(*----------------------------------------------------------------------------*)

print_string "\n--------------------------------- Hw1 --------------------------------\n\n";;

let rec print_list = function
    | [] -> ()
    | h::t -> print_int h;
              print_string " ";
              print_list t;;


(* peano *)
print_string "~~~~~~~~ peano ~~~~~~~~\n\n";;

(* peano_of_int *)
print_string "peano_of_int 2 = ";;
print_int (int_of_peano (S (S Z)));;

print_string "\n";;

(* int_of_peano *)
print_string "\nint_of_peano 2 = ";;
print_int (int_of_peano (peano_of_int 2));;

print_string "\n";;

(* inc *)
print_string "\ninc 3 = ";;
print_int (int_of_peano (inc (peano_of_int 3)));;

print_string "\n";;

(* add *)
print_string "\nadd 0 8 = ";;
print_int (int_of_peano (add (peano_of_int 0) (peano_of_int 8)));;

print_string "\nadd 8 3 = ";;
print_int (int_of_peano (add (peano_of_int 8) (peano_of_int 3)));;

print_string "\n";;

(* sub *)
print_string "\nsub 3 8 = ";;
print_int (int_of_peano (sub (peano_of_int 3) (peano_of_int 8)));;

print_string "\nsub 8 3 = ";;
print_int (int_of_peano (sub (peano_of_int 8) (peano_of_int 3)));;

print_string "\n";;

(* mul *)
print_string "\nmul 0 8 = ";;
print_int (int_of_peano (mul (peano_of_int 0) (peano_of_int 8)));;

print_string "\nmul 8 3 = ";;
print_int (int_of_peano (mul (peano_of_int 8) (peano_of_int 3)));;

print_string "\n";;

(* power *)
print_string "\npower 0 8 = ";;
print_int (int_of_peano (power (peano_of_int 0) (peano_of_int 8)));;

print_string "\npower 8 3 = ";;
print_int (int_of_peano (power (peano_of_int 8) (peano_of_int 3)));;

print_string "\n";;

(* div *)
print_string "\ndiv 0 8 = ";;
print_int (int_of_peano (div (peano_of_int 0) (peano_of_int 8)));;

print_string "\ndiv 9 3 = ";;
print_int (int_of_peano (div (peano_of_int 9) (peano_of_int 3)));;

print_string "\ndiv 8 3 = ";;
print_int (int_of_peano (div (peano_of_int 8) (peano_of_int 3)));;

print_string "\n\n";;

(* div by zero *)
(* print_string "\ndiv by zero: ";;
print_int (int_of_peano (div (peano_of_int 0) (peano_of_int 0)));; *)


(* rev and merge_sort *)
print_string "\n\n~~~~~~~~ rev and merge_sort ~~~~~~~~\n\n";;

(* rev *)
print_string "rev:\n";;
print_list (rev (1 :: 2 :: 3 :: 4 :: 5 :: []));;

print_string "\n";;
print_list (rev (rev (1 :: 2 :: 3 :: 4 ::5 ::[])));;

print_string "\n\n";;

(* merge_sort *)
print_string "merge_sort:\n";;
print_list (4 :: 5 :: 1 :: 2 :: 3 :: 7 :: 2 :: 2 :: 7 :: 6 :: 9 :: 8 ::[]);;

print_string "\n";;
print_list (merge_sort (4 :: 5 :: 1 :: 2 :: 3 :: 7 :: 2 :: 2 :: 7 :: 6 :: 9 :: 8 ::[]));;

print_string "\n\n";;


(* lambda *)
print_string "\n\n~~~~~~~~ lambda ~~~~~~~~\n\n";;

(* string_of_lambda *)
print_string "string_of_lambda:\n";;
print_string (string_of_lambda (App (Abs ("b", Var "a"), Var "c")));;

print_string "\n\n";;

(* lambda_of_string *)
print_string "lambda_of_string:\n";;
print_string "\\x.\\y.x = ";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x"));;

print_string "\n\\x.y z = ";;
print_string (string_of_lambda (lambda_of_string "\\x.y z"));;

print_string "\n\\a.(b) = ";;
print_string (string_of_lambda (lambda_of_string "\\a.(b)"));;

print_string "\n\\xeerf83eb.ejhdejd1822 111ndnen2 = ";;
print_string (string_of_lambda (lambda_of_string "\\xeerf83eb.ejhdejd1822 111ndnen2"));;

print_string "\na (b c) d e = ";;
print_string (string_of_lambda (lambda_of_string "a (b c) d e"));;

print_string "\n\\x.(a b \\y.z p) = ";;
print_string (string_of_lambda (lambda_of_string "\\x.(aaa bbb \\y.z p)"));;

print_string "\n\\x.\\y.x x x \\y.z = ";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x x x \\y.z"));;

print_string "\n(((x y)) \\a.b) = ";;
print_string (string_of_lambda (lambda_of_string "(((x y)) \\a.b)"));;

print_string "\n(\\a.\\b.\\c.t \\d.\\f.\\e.x) = ";;
print_string (string_of_lambda (lambda_of_string "(\\a.\\b.\\c.t \\d.\\f.\\e.x)"));;

print_string "\n\n";;

(*----------------------------------------------------------------------------*)

print_string "\n\n---------------------------- Hw1_reduction ---------------------------\n\n";;

(*----------------------------------------------------------------------------*)

print_string "\n\n------------------------------ Hw2_unify -----------------------------\n\n";;

let rec string_of_alg_term = function
    | Var name      -> name
    | Fun (name, l) ->
        "(" ^ name ^ " " ^ String.concat " " (List.map string_of_alg_term l) ^ ")";;

let print_alg_term at = print_string (string_of_alg_term at);;

let print_eq (left, right) =
    print_alg_term left;
    print_string " = ";
    print_alg_term right;;

let rec print_solution = function
    | x :: xs -> (print_string (fst x ^ " := " ^ string_of_alg_term (snd x) ^ "\n");
                  print_solution xs)
    | []      -> ();;

let print_verdict l syst =
    match check_solution l syst with
        | true -> print_string "ok"
        | _    -> print_string "fail";;


(* system_to_equation *)
print_string "~~~~~~~~ system_to_equation ~~~~~~~~\n\n";;

let a1 = Fun ("nnn", [Var "y"; Var "z"]);;
let a2 = Fun ("n", [a1; Var "xx"]);;
let ste_t1 =
    [(Var "a", a1);
     (a2, Var "bb")];;
let ste_t2 =
    [(Var "a", a2);
     (a1, Var "bbbb")];;

print_string "a = nnn y z\n";;
print_string "n (nnn y z) xx = bb\n";;
print_string "ans:\n";;
print_eq (system_to_equation ste_t1);;
print_string "\n\n";;

print_string "a = n (nnn y z) xx\n";;
print_string "nnn y z = bbbb\n";;
print_string "ans:\n";;
print_eq (system_to_equation ste_t2);;
print_string "\n\n";;


(* apply_substitution *)
print_string "\n\n~~~~~~~~ apply_substitution ~~~~~~~~\n\n";;

let st = Fun ("sf", [Var "sv1"; Var "sv2"]);;
let subst1 = [("bb", st)];;
let subst2 = [("z", st)];;

let a11 = Fun ("nn", [Var "bb"; Var "z"]);;
let a22 = Fun ("n", [a11; Var "z"]);;

print_string "nn bb z\n";;
print_string "bb := sf sv1 sv2\n";;
print_string "ans:\n";;
print_alg_term (apply_substitution subst1 a11);;
print_string "\n\n";;

print_string "n (nn bb z) z\n";;
print_string "z := sf sv1 sv2\n";;
print_string "ans:\n";;
print_alg_term (apply_substitution subst2 a22);;
print_string "\n\n";;


(* solve_system *)
print_string "\n\n~~~~~~~~ solve_system ~~~~~~~~\n\n";;

let at1r = Fun ("f", [Var "a"; Var "b"]);;
let at1 = Fun ("f", [Var "b"; at1r]);;
let at2l = Fun ("f", [Var "y"; Var "y"]);;
let at2 = Fun ("f", [at2l; Var "z"]);;

let test_solve_system syst =
    match solve_system syst with
        | Some l -> (print_solution l;
                     print_string "verdict:\n";
                     print_verdict l syst)
        | _      -> print_string "not solution";;

(* [("(f b (f a b))", "(f (f y y) z)")] *)
print_string "f b (f a b) = f (f y y) z\n";;
print_string "ans:\n";;
test_solve_system [(at1, at2)];;
print_string "\n\n";;

(* [("x","(f (f y y) z)"),
    ("x", "(f b (f a b))")] *)
print_string "x = f (f y y) z\n";;
print_string "x = f b (f a b)\n";;
print_string "ans:\n";;
test_solve_system [(Var "x", at2); (Var "x", at1)];;
print_string "\n\n";;

(*----------------------------------------------------------------------------*)

print_string "\n\n---------------------------- Hw2_inference ---------------------------\n\n";;

let rec string_of_simp_type = function
    | S_Elem s         -> s
    | S_Arrow (tl, tr) ->
        "(" ^ string_of_simp_type tl ^ " -> " ^ string_of_simp_type tr ^ ")";;

let print_simp_res (l, t) =
    let rec print_subst = function
        | x :: xs -> (print_string ("\n" ^ fst x ^ " := " ^ string_of_simp_type (snd x));
                      print_subst xs)
        | []      -> ()
    in
    print_string "\n";
    print_string "type:\n";
    print_string (string_of_simp_type t);
    print_string "\nsubstitution:";
    print_subst l;;

let rec string_of_hm_type = function
    | HM_Elem s         -> s
    | HM_Arrow (tl, tr) ->
        "(" ^ string_of_hm_type tl ^ " -> " ^ string_of_hm_type tr ^ ")"
    | HM_ForAll (s, t)  -> "V" ^ s ^ "." ^ string_of_hm_type t;;

let print_hm_res (l, t) =
    let rec print_subst = function
        | x :: xs -> (print_string ("\n" ^ fst x ^ " := " ^ string_of_hm_type (snd x));
                      print_subst xs)
        | []      -> ()
    in
    print_string "\n";
    print_string "type:\n";
    print_string (string_of_hm_type t);
    print_string "\nsubstitution:";
    print_subst l;;


(* infer_simp_type *)
print_string "~~~~~~~~ infer_simp_type ~~~~~~~~\n\n";;

let test_ist lam =
    match infer_simp_type lam with
        | Some res -> print_simp_res res
        | _        -> print_string ":(";;

(* \\x.x *)
print_string "\\x.x";;
let ist_t1 = (Abs ("x", Var "x"));;
test_ist ist_t1;;
print_string "\n\n";;

(* \\f.(\\x.(f x))*)
print_string "\\f.(\\x.(f x))";;
let ist_t2 = (Abs ("f", (Abs ("x", (App (Var "f", Var "x"))))));;
test_ist ist_t2;;
print_string "\n\n";;

(* \\f.(\\x.(f (f x))) *)
print_string "\\f.(\\x.(f (f x)))";;
let ist_t3 = (Abs ("f", (Abs ("x", (App (Var "f", (App (Var "f", Var "x"))))))));;
test_ist ist_t3;;
print_string "\n\n";;


(* algorithm_w *)
print_string "\n\n~~~~~~~~ algorithm_w ~~~~~~~~\n\n";;

let test_w lam =
    match algorithm_w lam with
        | Some res -> print_hm_res res
        | _        -> print_string ":(";;

(* \\x.x *)
print_string "\\x.x";;
let w_t1 = HM_Abs ("x", HM_Var "x");;
test_w w_t1;;
print_string "\n\n";;

(* \\f.(\\x.(f x)) *)
print_string "\\f.(\\x.(f x))";;
let w_t2 = HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", HM_Var "x")))));;
test_w w_t2;;
print_string "\n\n";;

(* \\f.(\\x.(f (f x))) *)
print_string "\\f.(\\x.(f (f x)))";;
let w_t3 = HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", (HM_App (HM_Var "f", HM_Var "x")))))));;
test_w w_t3;;
print_string "\n\n";;

(* let d = (\\x.x) in (\\f.(\\x.(d (d (d x))))) *)
print_string "let d = (\\x.x) in (\\f.(\\x.(d (d (d x)))))";;
let w_t4 = HM_Let ("d", (HM_Abs ("x", HM_Var "x")), (HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "d", (HM_App (HM_Var "d", (HM_App (HM_Var "d", HM_Var "x")))))))))));;
test_w w_t4;;
print_string "\n\n";;

(* let d = (\\t.t) in (\\f.(\\x.((d f) (d x)))) *)
print_string "let d = (\\t.t) in (\\f.(\\x.((d f) (d x))))";;
let w_t5 = HM_Let ("d", (HM_Abs ("t", HM_Var "t")), (HM_Abs ("f", (HM_Abs ("x", (HM_App ((HM_App (HM_Var "d", HM_Var "f")), (HM_App (HM_Var "d", HM_Var "x")))))))));;
test_w w_t5;;
print_string "\n\n";;

(* let d = (\\t.t) in (\\f.(\\x.((d f) ((d f) (d x))))) *)
print_string "let d = (\\t.t) in (\\f.(\\x.((d f) ((d f) (d x)))))";;
let w_t6 = HM_Let ("d", (HM_Abs ("t", HM_Var "t")), (HM_Abs ("f", (HM_Abs ("x", (HM_App ((HM_App (HM_Var "d", HM_Var "f")), (HM_App ((HM_App (HM_Var "d", HM_Var "f")), (HM_App (HM_Var "d", HM_Var "x")))))))))));;
test_w w_t6;;
print_string "\n\n";;

(* let w = (\\f.(\\x.(f (f x)))) in ((w (w (w (w (w w)))))) *)
print_string "let w = (\\f.(\\x.(f (f x)))) in ((w (w (w (w (w w))))))";;
let w_t7 = HM_Let ("w", (HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", (HM_App (HM_Var "f", HM_Var "x")))))))), (HM_App (HM_Var "w", (HM_App (HM_Var "w", (HM_App (HM_Var "w", (HM_App (HM_Var "w", (HM_App (HM_Var "w", HM_Var "w")))))))))));;
test_w w_t7;;
print_string "\n\n";;

(* let a = (\\f.(\\x.(f (f x)))) in (let b = (\\f.(\\x.(f (f x)))) in (let c = (\\f.(\\x.(f (f x)))) in ((a (b c))))) *)
print_string "let a = (\\f.(\\x.(f (f x)))) in (let b = (\\f.(\\x.(f (f x)))) in (let c = (\\f.(\\x.(f (f x)))) in ((a (b c)))))";;
let w_t8 = HM_Let ("a", (HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", (HM_App (HM_Var "f", HM_Var "x")))))))), (HM_Let ("b", (HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", (HM_App (HM_Var "f", HM_Var "x")))))))), (HM_Let ("c", (HM_Abs ("f", (HM_Abs ("x", (HM_App (HM_Var "f", (HM_App (HM_Var "f", HM_Var "x")))))))), (HM_App (HM_Var "a", (HM_App (HM_Var "b", HM_Var "c")))))))));;
test_w w_t8;;
print_string "\n\n";;
