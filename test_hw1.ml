open Hw1;;

print_string ("Tests:\n");;

(*peano_of_int*)
print_string ("\npeano_of_int 2 = ");;
print_int (int_of_peano (S (S (Z))));;

print_string("\n");;

(*int_of_peano*)
print_string ("\nint_of_peano 2 = ");;
print_int (int_of_peano (peano_of_int 2));;

print_string("\n");;

(*inc*)
print_string ("\ninc 3 = ");;
print_int (int_of_peano (inc (peano_of_int 3)));;

print_string("\n");;

(*add*)
print_string ("\nadd 0 8 = ");;
print_int (int_of_peano (add (peano_of_int 0) (peano_of_int 8)));;

print_string ("\nadd 8 3 = ");;
print_int (int_of_peano (add (peano_of_int 8) (peano_of_int 3)));;

print_string("\n");;

(*sub*)
print_string ("\nsub 3 8 = ");;
print_int (int_of_peano (sub (peano_of_int 3) (peano_of_int 8)));;

print_string ("\nsub 8 3 = ");;
print_int (int_of_peano (sub (peano_of_int 8) (peano_of_int 3)));;

print_string("\n");;

(*mul*)
print_string ("\nmul 0 8 = ");;
print_int (int_of_peano (mul (peano_of_int 0) (peano_of_int 8)));;

print_string ("\nmul 8 3 = ");;
print_int (int_of_peano (mul (peano_of_int 8) (peano_of_int 3)));;

print_string("\n");;

(*power*)
print_string ("\npower 0 8 = ");;
print_int (int_of_peano (power (peano_of_int 0) (peano_of_int 8)));;

print_string ("\npower 8 3 = ");;
print_int (int_of_peano (power (peano_of_int 8) (peano_of_int 3)));;

print_string ("\n");;

(*div*)
print_string ("\ndiv 0 8 = ");;
print_int (int_of_peano (div (peano_of_int 0) (peano_of_int 8)));;

print_string ("\ndiv 9 3 = ");;
print_int (int_of_peano (div (peano_of_int 9) (peano_of_int 3)));;

print_string ("\ndiv 8 3 = ");;
print_int (int_of_peano (div (peano_of_int 8) (peano_of_int 3)));;

print_string ("\n\n");;

(*div by zero*)
(*print_string ("\ndiv by zero: ");;
print_int (int_of_peano (div (peano_of_int 0) (peano_of_int 0)));;*)

(*rev*)
print_string("rev:\n");;
print_list (rev (1 :: 2 :: 3 :: 4 :: 5 :: []));;

print_string("\n");;
print_list (rev (rev (1 :: 2 :: 3 :: 4 ::5 ::[])));;

print_string("\n\n");;

(*merge_sort*)
print_string("merge_sort:\n");;
print_list (4 :: 5 :: 1 :: 2 :: 3 :: 7 :: 2 :: 2 :: 7 :: 6 :: 9 :: 8 ::[]);;

print_string("\n");;
print_list (merge_sort (4 :: 5 :: 1 :: 2 :: 3 :: 7 :: 2 :: 2 :: 7 :: 6 :: 9 :: 8 ::[]));;

print_string("\n\n");;

(*string_of_lambda*)
print_string("string_of_lambda:\n");;
print_string(string_of_lambda (App((Abs ("b",(Var "a"))), (Var "c"))));;

print_string("\n\n");;

(*lambda_of_string*)
print_string("lambda_of_string:\n");;
print_string("\\x.\\y.x = ");;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x"));;

print_string("\n\\x.y z = ");;
print_string (string_of_lambda (lambda_of_string "\\x.y z"));;

print_string("\n\\a.(b) = ");;
print_string (string_of_lambda (lambda_of_string "\\a.(b)"));;

print_string("\n\\xeerf83eb.ejhdejd1822 111ndnen2 = ");;
print_string (string_of_lambda (lambda_of_string "\\xeerf83eb.ejhdejd1822 111ndnen2"));;

print_string("\na (b c) d e = ");;
print_string (string_of_lambda (lambda_of_string "a (b c) d e"));;

print_string("\n\\x.(a b \\y.z p) = ");;
print_string (string_of_lambda (lambda_of_string "\\x.(aaa bbb \\y.z p)"));;

print_string("\n\\x.\\y.x x x \\y.z = ");;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x x x \\y.z"));;

print_string("\n(((x y)) \\a.b) = ");;
print_string (string_of_lambda (lambda_of_string "(((x y)) \\a.b)"));;

print_string("\n(\\a.\\b.\\c.t \\d.\\f.\\e.x) = ");;
print_string (string_of_lambda (lambda_of_string "(\\a.\\b.\\c.t \\d.\\f.\\e.x)"));;

print_string("\n\n");;
