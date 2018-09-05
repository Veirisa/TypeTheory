open Hw1

module S = Set.Make(String)
module M = Map.Make(String)

let find_with_default d k m =
    match M.find_opt k m with
        | Some el -> el
        | _       -> d;;

(*----------------------------------------------------------------------------*)

let rec free_vars_set = function
    | Var s        -> S.singleton s
    | Abs (s, l)   -> S.remove s (free_vars_set l)
    | App (l1, l2) -> S.union (free_vars_set l1) (free_vars_set l2);;


let free_vars l = S.elements (free_vars_set l);;

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


let free_to_subst sl l x = S.is_empty (fails_free_to_subst sl l x);;

(*----------------------------------------------------------------------------*)

let rec is_normal_form = function
    | App (Abs (_, _), _) -> false
    | App (l1, l2)        -> is_normal_form l1 && is_normal_form l2
    | Abs (_, l)          -> is_normal_form l
    | _                   -> true;;

(*----------------------------------------------------------------------------*)

let is_alpha_equivalent =

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

    check_alpha_eq 0 M.empty M.empty;;

(*----------------------------------------------------------------------------*)

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

let name_partition full_name =

    let rec part_with_num_in_rev_list acc_num name =
        match name with
            | c :: cs ->
                if c >= '0' && c <= '9'
                then part_with_num_in_rev_list (c :: acc_num) cs
                else (String.of_seq (List.to_seq (rev name)),
                      String.of_seq (List.to_seq acc_num))
            | [] -> ("", String.of_seq (List.to_seq acc_num))
    in

    let rev_name_list = rev (List.of_seq (String.to_seq full_name)) in
    part_with_num_in_rev_list [] rev_name_list;;


let rec convert_to_block_map block l =
    match l with
        | [] -> block
        | x :: xs ->
            let (pr_name, s_num) = name_partition x in
            let num = if String.length s_num = 0 then 0 else int_of_string s_num + 1 in
            let new_block =
                match M.find_opt pr_name block with
                    | Some old_num -> M.add pr_name (max old_num num) block
                    | _            -> M.add pr_name num block
            in
            convert_to_block_map new_block xs;;


let rec get_all_names = function
    | Var s        -> S.singleton s
    | Abs (s, l)   -> S.union (S.singleton s) (get_all_names l)
    | App (l1, l2) -> S.union (get_all_names l1) (get_all_names l2);;


let rec get_abs_names = function
    | Var _        -> S.empty
    | Abs (s, l)   -> S.union (S.singleton s) (get_abs_names l)
    | App (l1, l2) -> S.union (get_abs_names l1) (get_abs_names l2);;


let get_fails_abs_names l =

    let rec get_var_names = function
        | Var s        -> S.singleton s
        | Abs (_, l)   -> get_var_names l
        | App (l1, l2) -> S.union (get_var_names l1) (get_var_names l2)
    in

    S.inter (get_var_names l) (get_var_names l);;


let create_new_name block s =
    let (pr_name, _) = name_partition s in
    match M.find_opt pr_name block with
        | Some num -> (pr_name ^ string_of_int num, M.add pr_name (num + 1) block)
        | _ -> (pr_name, M.add pr_name 0 block);;


let rec rename_fail_abs block fails m l =
    match l with
        | Var s ->
            (match M.find_opt s m with
                 | Some new_s -> (Var new_s, block)
                 | _          -> (l, block))
        | Abs (s', l') ->
            let (new_block1, new_m) =
                if S.mem s' fails
                then
                    let (new_name, new_block) = create_new_name block s' in
                    (new_block, M.add s' new_name m)
                else (block, m)
            in
            let (new_l, new_block2) = rename_fail_abs new_block1 fails new_m l' in
            (Abs (find_with_default s' s' new_m, new_l), new_block2)
        | App (l1, l2) ->
            let (new_l1, new_block1) = rename_fail_abs block fails m l1 in
            let (new_l2, new_block2) = rename_fail_abs new_block1 fails m l2 in
            (App (new_l1, new_l2), new_block2);;

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

(*----------------------------------------------------------------------------*)

let substitution block sl l x =

    let rec free_substitution sl l x =
        match l with
            | Var s        -> if s = x then sl else l
            | Abs (s', l') -> if s' = x then l else Abs (s', free_substitution sl l' x)
            | App (l1, l2) -> App (free_substitution sl l1 x, free_substitution sl l2 x)
    in

    let fail_vars = fails_free_to_subst sl l x in
    let (new_l, new_block) = rename_fail_abs block fail_vars M.empty l in
    (free_substitution sl new_l x, new_block);;


let rec reduction block l =
    match l with
        | App (Abs (s1', l1'), l2) ->
            let (new_l, _) = substitution block l2 l1' s1' in
            (new_l, true)
        | App (l1, l2) ->
            (match reduction block l1 with
                 | (new_l1, true) -> (App (new_l1, l2), true)
                 | _ ->
                     (match reduction block l2 with
                          | (new_l2, true) -> (App (l1, new_l2), true)
                          | _              -> (l, false)))
        | Abs (s', l') ->
            (match reduction block l' with
                 | (new_l', true) -> (Abs (s', new_l'), true)
                 | _              -> (l, false))
        | Var _ -> (l, false);;


let normal_beta_reduction l =
    let block = convert_to_block_map M.empty (S.elements (get_all_names l)) in
    fst (reduction block l)

(*----------------------------------------------------------------------------*)

let rec slow_reduce_to_normal_form l =
    match reduction (convert_to_block_map M.empty (S.elements (get_all_names l))) l with
        | (new_l, true) -> slow_reduce_to_normal_form new_l
        | _             -> l

(*----------------------------------------------------------------------------*)

let rec smart_reduction block m_l is_left l =
    match l with
        | App (Abs (s1', l1'), l2) ->
            (l1', true, block, M.add s1' (l2, false) m_l)
        | App (l1, l2) ->
            (match smart_reduction block m_l true l1 with
                 | (new_l1, true, new_block, new_m_l) ->
                     (App (new_l1, l2), true, new_block, new_m_l)
                 | _ ->
                     (match smart_reduction block m_l false l2 with
                          | (new_l2, true, new_block, new_m_l) ->
                               (App (l1, new_l2), true, new_block, new_m_l)
                          | _ -> (l, false, block, m_l)))
        | Abs (s', l') ->
            (match smart_reduction block m_l false l' with
                 | (new_l', true, new_block, new_m_l) ->
                     (Abs (s', new_l'), true, new_block, new_m_l)
                 | _ -> (l, false, block, m_l))
        | Var s ->
            (match (M.find_opt s m_l, is_left) with
                 (* мы - левый ребенок App и являемся Abs => return *)
                 (* кладем: ничего, ибо ничего в лямбде не поменяли *)
                 | (Some (Abs (l1, l2), false), true) ->
                     let real_l = Abs (l1, l2) in
                     let (un_real_l, un_block) = rename_fail_abs block (get_abs_names real_l) M.empty real_l in
                     (un_real_l, true, un_block, m_l)
                 (* мы - левый ребенок App и не являемся Abs => редуцируем до Abs (если повезет - до норм формы) *)
                 | (Some (real_l, false), true) ->
                     (match do_smart_reduction_to_abs block m_l true real_l with
                         (* мы доредуцировали до норм формы => return *)
                         (* кдадем: Лямбда + true *)
                         | (new_l, false, new_block, new_m_l) ->
                             let (un_new_l, un_new_block) = rename_fail_abs new_block (get_abs_names new_l) M.empty new_l in
                             (un_new_l, true, un_new_block, M.add s (new_l, true) new_m_l)
                         (* мы доредуцировали до Abs => return *)
                         (* кладем: Лямбда + false, ибо не факт, что мы в норм форме *)
                         | (new_l, _, new_block, new_m_l) ->
                             let (un_new_l, un_new_block) = rename_fail_abs new_block (get_abs_names new_l) M.empty new_l in
                             (un_new_l, true, un_new_block, M.add s (new_l, false) new_m_l))
                 (* выше не может появиться бета-редекса => редуцируем до норм формы и return *)
                 (* кладем: Лямбда + true *)
                 | (Some (real_l, false), false) ->
                     let (new_l, _, new_block, new_m_l) = do_smart_reduction_to_norm block m_l false real_l in
                     let (un_new_l, un_new_block) = rename_fail_abs new_block (get_abs_names new_l) M.empty new_l in
                     (un_new_l, true, un_new_block, M.add s (new_l, true) new_m_l)
                 (* наша прошлая копия уже доредуцирована => return *)
                 (* кладем: ничего, ибо и так лежит лямбда в норм форме *)
                 | (Some (real_l, true), _) ->
                     let (un_real_l, un_block) = rename_fail_abs block (get_abs_names real_l) M.empty real_l in
                     (un_real_l, true, un_block, m_l)
                 (* эта переменная не является лениво спрятанной лямбдой => return *)
                 | _ -> (l, false, block, m_l));;

let rec do_smart_reduction_to_abs = failwith "nr";;
let rec do_smart_reduction_to_norm = failwith "nr";;

let reduce_to_normal_form = slow_reduce_to_normal_form;;
