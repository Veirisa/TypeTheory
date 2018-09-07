open Hw1
open Hw2_unify

module S = Set.Make(String)
module M = Map.Make(String)


let left_pr = fun k l _ -> Some l;;


let pair_map f (l, r) = (l, f r);;


let find_with_default d k m =
    match M.find_opt k m with
        | Some el -> el
        | _       -> d;;


let map_from_list l =

    let rec create_map m l =
        match l with
            | x :: xs -> create_map (M.add (fst x) (snd x) m) xs
            | _       -> m
    in

    create_map M.empty l;;

(*----------------------------------------------------------------------------*)

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

type simp_type = S_Elem of string
               | S_Arrow of simp_type * simp_type


let rec string_of_simp_type = function
    | S_Elem s         -> s
    | S_Arrow (tl, tr) ->
        "(" ^ string_of_simp_type tl ^ " -> " ^ string_of_simp_type tr ^ ")";;


let rec alg_term_of_simp_type = function
    | S_Elem s         -> Var s
    | S_Arrow (tl, tr) ->
        Fun ("a", [alg_term_of_simp_type tl; alg_term_of_simp_type tr]);;


let rec simp_type_of_alg_term = function
    | Var s             -> S_Elem s
    | Fun (n, [tl; tr]) ->
        if n = "a"
        then S_Arrow (simp_type_of_alg_term tl, simp_type_of_alg_term tr)
        else failwith "incorrect algebraic term (func name)"
    | _                 -> failwith "incorrect algebraic term (func args)";;

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

(*----------------------------------------------------------------------------*)

let infer_simp_type l =

    let gen_simp_type num = S_Elem ("t" ^ string_of_int num)
    in

    let rec create_system m num l =
        match l with
            | App (l1, l2) ->
                let (syst1, st1, m1) = create_system m (2 * num + 1) l1 in
                let (syst2, st2, m2) = create_system m1 (2 * num + 2) l2 in
                let new_st = gen_simp_type num in
                let new_eq = (st1, S_Arrow (st2, new_st)) in
                (new_eq :: syst1 @ syst2, new_st, M.union left_pr m1 m2)
            | Abs (s, l) ->
                let opt_old_st_s = M.find_opt s m in
                let new_st_s = gen_simp_type num in
                let m_with_s = M.add s new_st_s m in
                let (syst, st, m') = create_system m_with_s (2 * num + 1) l in
                let new_st = S_Arrow (new_st_s, st) in
                (match opt_old_st_s with
                     | Some old_st_s -> (syst, new_st, M.add s old_st_s m')
                     | _             -> (syst, new_st, M.remove s m'))
            | Var s ->
                (match M.find_opt s m with
                     | Some st_s -> ([], st_s, m)
                     | _         ->
                         let new_st_s = gen_simp_type num in
                         ([], new_st_s, M.add s new_st_s m))
    in

    let (syst, st, _) = create_system M.empty 0 l in
    let convert_to_eq = fun (st1, st2) -> (alg_term_of_simp_type st1, alg_term_of_simp_type st2) in
    match solve_system (List.map convert_to_eq syst) with
        | Some solut ->
            Some (List.map (pair_map simp_type_of_alg_term) solut,
                  simp_type_of_alg_term (apply_substitution solut (alg_term_of_simp_type st)))
        | _ -> None;;

(*----------------------------------------------------------------------------*)

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

type hm_lambda = HM_Var of string
               | HM_Abs of string * hm_lambda
               | HM_App of hm_lambda * hm_lambda
               | HM_Let of string * hm_lambda * hm_lambda


type hm_type = HM_Elem of string
             | HM_Arrow of hm_type * hm_type
             | HM_ForAll of string * hm_type


let rec string_of_hm_type = function
    | HM_Elem s         -> s
    | HM_Arrow (tl, tr) ->
        "(" ^ string_of_hm_type tl ^ " -> " ^ string_of_hm_type tr ^ ")"
    | HM_ForAll (s, t)  -> "V" ^ s ^ "." ^ string_of_hm_type t;;


let rec alg_term_of_hm_type = function
    | HM_Elem s         -> Var s
    | HM_Arrow (tl, tr) ->
        Fun ("a", [alg_term_of_hm_type tl; alg_term_of_hm_type tr])
    | _                 -> failwith "hm_type with ForAll can not be converted";;


let rec hm_type_of_alg_term = function
    | Var s             -> HM_Elem s
    | Fun (n, [tl; tr]) ->
        if n = "a"
        then HM_Arrow (hm_type_of_alg_term tl, hm_type_of_alg_term tr)
        else failwith "incorrect algebraic term (func name)"
    | _                 -> failwith "incorrect algebraic term (func args)";;

(* /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\ *)

(*----------------------------------------------------------------------------*)

let gen_hm_type num = HM_Elem ("t" ^ string_of_int num);;


let hm_lambda_free_vars_map lam =

    let gen_free_var_type num = HM_Elem ("fvt" ^ string_of_int num)
    in

    let rec gen_hm_lambda_free_vars_map num lam =
        match lam with
            | HM_Var s -> M.singleton s (gen_free_var_type num)
            | HM_App (l1, l2) ->
                let m1 = gen_hm_lambda_free_vars_map (2 * num + 1) l1 in
                let m2 = gen_hm_lambda_free_vars_map (2 * num + 2) l2 in
                M.union left_pr m1 m2
            | HM_Abs (s, l) ->
                M.remove s (gen_hm_lambda_free_vars_map (2 * num + 1) l)
            | HM_Let (s, l1, l2) ->
                let m1 = gen_hm_lambda_free_vars_map (2 * num + 1) l1 in
                let m2 =  M.remove s (gen_hm_lambda_free_vars_map (2 * num + 2) l2) in
                M.union left_pr m1 m2
    in

    gen_hm_lambda_free_vars_map 0 lam;;


let rec apply_type_substitution m typ =
    match typ with
        | HM_Elem s -> find_with_default typ s m
        | HM_Arrow (tl, tr) ->
            HM_Arrow (apply_type_substitution m tl, apply_type_substitution m tr)
        | HM_ForAll (s, t) ->
            if M.mem s m
            then apply_type_substitution m t
            else HM_ForAll (s, apply_type_substitution m t);;


let apply_multi_types_substitution subst = M.map (apply_type_substitution subst);;


let depend_types_substitution num t =

    let rec hm_type_depend_vars = function
        | HM_ForAll (s, t) -> s :: hm_type_depend_vars t
        | _                -> []
    in

    let rec gen_var_type_map num vtl =
        match vtl with
           | []      -> (M.empty, num)
           | x :: xs ->
               let (new_m, new_num) = gen_var_type_map (num + 1) xs in
               (M.add x (gen_hm_type num) new_m, new_num)
    in

    let depend_vars = hm_type_depend_vars t in
    let (new_var_types_map, new_num) = gen_var_type_map num depend_vars in
    (apply_type_substitution new_var_types_map t, new_num);;


let closure cont t =

    let rec hm_type_free_vars_set = function
        | HM_Elem s         -> S.singleton s
        | HM_Arrow (tl, tr) ->
            S.union (hm_type_free_vars_set tl) (hm_type_free_vars_set tr)
        | HM_ForAll (s, t)  ->
            S.remove s (hm_type_free_vars_set t)
    in

    let rec hm_context_type_free_vars_set = function
        | []      -> S.empty
        | t :: ts ->
            S.union (hm_context_type_free_vars_set ts) (hm_type_free_vars_set t)
    in

    let rec add_dependence t vl =
        match vl with
            | []      -> t
            | x :: xs -> HM_ForAll (x, add_dependence t xs)
    in

    let cont_free_vars_set = hm_context_type_free_vars_set (List.map snd (M.bindings cont)) in
    let t_free_vars_set = hm_type_free_vars_set t in
    add_dependence t (S.elements (S.diff t_free_vars_set cont_free_vars_set));;


let compose next prev =
    M.union left_pr (apply_multi_types_substitution next prev) next;;


let type_unify left right =
    match solve_system [(alg_term_of_hm_type left, alg_term_of_hm_type right)] with
        | Some solut -> Some (map_from_list (List.map (pair_map hm_type_of_alg_term) solut))
        | _          -> None;;


let algorithm_w lam =

    let rec do_algorithm_w num cont lam =
        match lam with
            | HM_Var s ->
                (match M.find_opt s cont with
                     | None -> None
                     | Some s_type ->
                         let new_subst = M.empty in
                         let (new_type, new_num) = depend_types_substitution num s_type in
                         Some (new_subst, new_type, new_num))
            | HM_App (l1, l2) ->
                (match do_algorithm_w num cont l1 with
                     | None -> None
                     | Some (subst1, type1, num1) ->
                         let cont1 = apply_multi_types_substitution subst1 cont in
                         (match do_algorithm_w num1 cont1 l2 with
                              | None -> None
                              | Some (subst2, type2, num2) ->
                                  let beta = gen_hm_type num2 in
                                  let left = apply_type_substitution subst2 type1 in
                                  let right = HM_Arrow (type2, beta) in
                                  (match type_unify left right with
                                       | None -> None
                                       | Some v ->
                                           let new_subst = compose v (compose subst1 subst2) in
                                           let new_type = apply_type_substitution new_subst beta in
                                           let new_num = num2 + 1 in
                                           Some (new_subst, new_type, new_num))))
            | HM_Abs (s, l) ->
                let beta = gen_hm_type num in
                let cont0 = M.add s beta cont in
                let num0 = num + 1 in
                (match do_algorithm_w num0 cont0 l with
                     | None -> None
                     | Some (subst1, type1, num1) ->
                         let new_subst = subst1 in
                         let new_type = HM_Arrow (apply_type_substitution new_subst beta, type1) in
                         let new_num = num1 in
                         Some (new_subst, new_type, new_num))
            | HM_Let (s, l1, l2) ->
                (match do_algorithm_w num cont l1 with
                     | None -> None
                     | Some (subst1, type1, num1) ->
                         let changed_cont = apply_multi_types_substitution subst1 cont in
                         let changed_s_type = closure changed_cont type1 in
                         let cont1 = M.add s changed_s_type changed_cont in
                         (match do_algorithm_w num1 cont1 l2 with
                              | None -> None
                              | Some (subst2, type2, num2) ->
                                  let new_subst = compose subst2 subst1 in
                                  let new_type = type2 in
                                  let new_num = num2 in
                                  Some (new_subst, new_type, new_num)))
    in

    match do_algorithm_w 0 (hm_lambda_free_vars_map lam) lam with
        | Some (subst_map, t, _) -> Some (M.bindings subst_map, t)
        | _                      -> None;;
