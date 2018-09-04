open Hw1

module S = Set.Make(String)
module M = Map.Make(String)

(*----------------------------------------------------------------------------*)

let rec free_vars_set = function
    | Var s        -> S.singleton s
    | Abs (s, l)   -> S.remove s (free_vars_set l)
    | App (l1, l2) -> S.union (free_vars_set l1) (free_vars_set l2)


let free_vars l = S.elements (free_vars_set l)

(*----------------------------------------------------------------------------*)

let fails_free_to_subst sl l x =

    let rec get_fails_free_to_subst set lam x =
        match lam with
            | Var s -> (s = x, S.empty)
            | Abs (s, l) ->
                let (was, fails) = get_fails_free_to_subst set l x in
                let have_new_fail = was && S.mem s set in
                (was, if have_new_fail then S.add s fails else fails)
            | App (l1, l2) ->
                let (was1, fails1) = get_fails_free_to_subst set l1 x in
                let (was2, fails2) = get_fails_free_to_subst set l2 x in
                (was1 || was2, S.union fails1 fails2)
    in

    snd (get_fails_free_to_subst (S.of_list (free_vars sl)) l x);;


let free_to_subst sl l x = S.is_empty (fails_free_to_subst sl l x) ;;

(*----------------------------------------------------------------------------*)

let rec is_normal_form = function
    | App (Abs (_, _), _) -> false
    | App (l1, l2)        -> is_normal_form l1 && is_normal_form l2
    | Abs (_, l)          -> is_normal_form l
    | _                   -> true

(*----------------------------------------------------------------------------*)

let is_alpha_equivalent =

    let find_with_default d k m =
        match M.find_opt k m with
            | Some el -> el
            | _       -> d
    in

    let rec check_alpha_eq num m1 m2 lam1 lam2 =
        match (lam1, lam2) with
            | (Var s1, Var s2) ->
                find_with_default s1 s1 m1 = find_with_default s2 s2 m2
            | (Abs (s1, l1), Abs (s2, l2)) ->
                let new_s = "." ^ string_of_int num in
                let new_num = num + 1 in
                let new_m1 = M.add s1 new_s m1 in
                let new_m2 = M.add s2 new_s m2 in
                check_alpha_eq new_num new_m1 new_m2 l1 l2
            | (App (l1, l1'), App (l2, l2')) ->
                check_alpha_eq num m1 m2 l1 l2 && check_alpha_eq num m1 m2 l1' l2'
            | _ -> false
    in

    check_alpha_eq 0 M.empty M.empty

(*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*)

let normal_beta_reduction x = failwith "Not implemented";;

(*----------------------------------------------------------------------------*)

let reduce_to_normal_form x = failwith "Not implemented";;
