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

    let unique_name l' =
        String.make (maximum 0 (List.map get_pair_max_length l') + 1) 'f'
    in

    let name = unique_name l in
    (Fun (name, List.map fst l), Fun (name, List.map snd l));;

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
        | _                       -> failwith "incorrect equation in solution"
    in

    let rec contains_var x at =
        match at with
            | Var y   -> x = y
            | Fun (_, l) -> any (contains_var x) l
    in

    let try_substitution x new_at at =
        if contains_var x at
        then apply_substitution [(x, new_at)] at
        else at
    in

    let substitution x new_at ((at1, at2), flag) =
        let left = try_substitution x new_at at1 in
        let right = try_substitution x new_at at2 in
        ((left, right), flag)
    in

    let rec create_eqs l l1 l2 =
        match (l1, l2) with
            | (x :: xs, y :: ys) -> create_eqs (((x, y), false) :: l) xs ys
            | _                  -> List.rev l
    in

    let rec solve = function
        | ((Fun (n, l), Var x), _) :: eqs -> solve (((Var x, Fun (n, l)), false) :: eqs)
        | ((Fun (n1, l1), Fun (n2, l2)), _) :: eqs ->
            if (n1 <> n2) || (List.length l1 <> List.length l2)
            then None
            else solve_global (create_eqs [] l1 l2 @ eqs)
        | ((Var x, at), _) :: eqs  ->
            if contains_var x at
            then None
            else solve_global (List.map (substitution x at) eqs @ [((Var x, at), true)])
        | _ -> Some []

    and solve_global l_eq =
        match l_eq with
            | (_, true) :: _         -> Some l_eq
            | ((at1, at2), _) :: eqs ->
                if at1 = at2
                then solve_global eqs
                else solve l_eq
            | _                      -> Some []
    in

    match solve_global (List.map (fun eq -> (eq, false)) l_eq) with
        | Some result -> Some (List.map convert result)
        | _           -> None;;
