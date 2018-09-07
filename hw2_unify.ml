module S = Set.Make(String)
module M = Map.Make(String)


let create_map l =

    let rec multi_add_in_map m l =
        match l with
            | x :: xs -> multi_add_in_map (M.add (fst x) (snd x) m) xs
            | _       -> m

    in

    multi_add_in_map M.empty l;;


let any pr l =

    let rec search_true res l =
        match l with
            | x :: xs -> search_true (res || x) xs
            | _       -> res
    in

    search_true false (List.map pr l);;

(*----------------------------------------------------------------------------*)

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

type algebraic_term = Var of string
                    | Fun of string * (algebraic_term list)


let rec string_of_alg_term = function
    | Var name      -> name
    | Fun (name, l) ->
        "(" ^ name ^ " " ^ String.concat " " (List.map string_of_alg_term l) ^ ")";;

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

(*----------------------------------------------------------------------------*)

let system_to_equation l =

    let rec maximum res int_l =
        match int_l with
            | x :: xs -> maximum (max res x) xs
            | _         -> res
    in

    let rec get_max_length = function
        | Var x       -> String.length x
        | Fun (n, l') -> maximum (String.length n) (List.map get_max_length l')
    in

    let get_pair_max_length (left, right) =
        max (get_max_length left) (get_max_length right)
    in

    let uniqueName l' =
        String.make (maximum 0 (List.map get_pair_max_length l') + 1) 'f'
    in

    let name = uniqueName l in
    (Fun (name, List.map fst l), Fun (name, List.map snd l));;

(* let print_pair (left, right) = print_string (string_of_alg_term left); print_string "\n";
                               print_string (string_of_alg_term right); print_string "\n\n";;

let l1 = Fun ("na", [Var "y"; Var "z"]);;
let l2 = Fun ("t", [l1 ; Var "aaa"]);;
let ttt = [
          (Var "x", l1) ;
          (l2, Var "mr") ] ;;

print_pair (system_to_equation ttt);; *)

(*----------------------------------------------------------------------------*)

let apply_substitution l at =

    let rec substitution m at =
        match at with
            | Fun (n, l') -> Fun (n, List.map (substitution m) l')
            | Var x ->
                (match M.find_opt x m with
                     | Some new_at -> new_at
                     | _           -> at)
    in

    substitution (create_map l) at;;

(*----------------------------------------------------------------------------*)

let check_solution l_sub l_eq =
    let (left, right) = system_to_equation l_eq in
    apply_substitution l_sub left = apply_substitution l_sub right;;

(*----------------------------------------------------------------------------*)

let solve_system l_eq =

    let convert = function
        | ((Var x, right), true)  -> (x, right)
        | _                       -> failwith "incorrect situation"
    in

    let rec contains_var x at =
        match at with
            | Var y   -> x = y
            | Fun (_, l) -> any (contains_var x) l
    in

    let substitution l_sub ((at1, at2), _) =
        ((apply_substitution l_sub at1, apply_substitution l_sub at2), false)
    in

    let rec create_eqs l l1 l2 =
        match (l1, l2) with
            | (x :: xs, y :: ys) -> create_eqs (((x, y), false) :: l) xs ys
            | _                  -> List.rev l
    in

    let rec solve_global l_eq =
        match l_eq with
            | (_, true) :: _         -> Some l_eq
            | ((at1, at2), _) :: eqs ->
                if at1 = at2
                then solve_global eqs
                else solve l_eq
            | _                      -> Some []

    and solve = function
        | ((Fun (n, l), Var x), _) :: eqs -> solve (((Var x, Fun (n, l)), false) :: eqs)
        | ((Fun (n1, l1), Fun (n2, l2)), _) :: eqs ->
            if (n1 <> n2) || (List.length l1 <> List.length l2)
            then None
            else solve_global (create_eqs [] l1 l2 @ eqs)
        | ((Var x, at), _) :: eqs  ->
            let part_func = fun ((at1, at2), _) -> contains_var x at1 || contains_var x at2 in
            let (have, not_have) = List.partition part_func eqs in
            let (have_false, have_true) = List.partition (fun (_, b) -> not b) have in
            let (not_have_false, not_have_true) = List.partition (fun (_, b) -> not b) not_have in
            if contains_var x at
            then None
            else solve_global (List.map (substitution [(x, at)]) have_false @ not_have_false @
                               List.map (substitution [(x, at)]) have_true @ not_have_true @
                               [((Var x, at), true)])
        | _ -> Some []
    in

    match solve_global (List.map (fun eq -> (eq, false)) l_eq) with
        | Some result -> Some (List.map convert result)
        | _           -> None;;


(* [("(f b (f a b))", "(f (f y y) z)")]         *)

(* [("x","(f (f y y) z)"),
    ("x", "(f b (f a b))")] *)

(* let at1r = Fun ("f", [Var "a"; Var "b"]);;
let at1 = Fun ("f", [Var "b"; at1r]);;
let at2l = Fun ("f", [Var "y"; Var "y"]);;
let at2 = Fun ("f", [at2l; Var "z"]);;


let rec print_solution = function
    | x :: xs -> (print_string ("(" ^ fst x ^ ", " ^ string_of_alg_term (snd x) ^ ")\n");
                  print_solution xs)
    | [] -> print_string "";;

let check_solve syst =
    match solve_system syst with
        | Some l -> print_solution l; print_string "\n";
        | _ -> print_string "not solution\n\n";;

check_solve [(at1, at2)];;
check_solve [(Var "x", at2); (Var "x", at1)];; *)
