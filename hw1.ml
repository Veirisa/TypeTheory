(*----------------------------------------------------------------------------*)

type peano = Z
           | S of peano


let rec peano_of_int = function
    | 0 -> Z
    | x -> S (peano_of_int (x - 1));;


let rec int_of_peano = function
    | Z -> 0
    | S x -> 1 + int_of_peano x;;


let inc x = S x;;


let rec add x y =
    match y with
        | Z -> x
        | S yy -> add (inc x) yy;;


let rec sub x y =
    match (x, y) with
        | (Z, _) -> Z
        | (x, Z) -> x
        | (S xx, S yy) -> sub xx yy;;


let rec mul x y =
    match (x, y) with
        | (Z, _) -> Z
        | (_, Z) -> Z
        | (x, S yy) -> add x (mul x yy);;


let rec div x y =
    match (x, y, sub (inc x) y) with
        | (_, Z, _) -> failwith "div by zero"
        | (Z, _, _) -> Z
        | (_, _, Z) -> Z
        | (_, y, S ss) -> inc (div ss y);;


let rec power x y =
    match (x, y) with
        | (_, Z) -> S Z
        | (Z, _) -> Z
        | (x, S yy) -> mul x (power x yy);;

(*----------------------------------------------------------------------------*)

let rec do_rev x rev_l =
    match x with
        | [] -> rev_l
        | h :: t -> do_rev t (h :: rev_l);;


let rev x = do_rev x [];;


let rec merge x y ans =
    match (x, y) with
        | ([], []) -> rev ans
        | ([], y_h :: y_t) -> merge [] y_t (y_h :: ans)
        | (x_h :: x_t, []) -> merge x_t [] (x_h :: ans)
        | (x_h :: x_t, y_h :: y_t) ->
            if x_h < y_h
            then merge x_t (y_h :: y_t) (x_h :: ans)
            else merge (x_h :: x_t) y_t (y_h :: ans);;


let rec split l buf1 buf2 =
    match l with
        | [] -> (buf1, buf2)
        | h :: t -> split t (h :: buf2) buf1;;


let rec merge_parse = function
    | ([], y) -> y
    | (x, []) -> x
    | (x, y) -> merge (merge_parse (split x [] [])) (merge_parse (split y [] [])) [];;


let merge_sort x = merge_parse (split x [] []);;

(*----------------------------------------------------------------------------*)

type lambda = Var of string
            | Abs of string * lambda
            | App of lambda * lambda


let rec string_of_lambda = function
    | Var s -> s;
    | Abs (s, l) -> "(\\" ^ s ^ "." ^ (string_of_lambda l) ^ ")"
    | App (l1, l2) -> "(" ^ (string_of_lambda l1) ^ " " ^ (string_of_lambda l2) ^ ")";;


let lambda_of_string s =
    let s = s ^ ";" in
    let pos = ref 0 in
    let get() = s.[!pos] in

    let next() =
        if !pos < String.length s - 1
        then pos:= !pos + 1
        else failwith "range exception"
    in

    let eat x =
        if get() <> x
        then failwith "wrong food"
        else next()
    in

    let rec parse_name name =
        match (get()) with
            | '.' | ')' | ' ' | ';' -> name
            | _ ->
                (let sym = get() in
                 next();
                 parse_name (name ^ String.make 1 sym))
    in

    let parse_var() = Var (parse_name "") in

    let rec parse_lambda() =
        let left =
            match (get()) with
                | '\\' -> parse_abs()
                | '(' ->
                    (eat '(';
                     let res = parse_lambda() in
                     eat ')';
                     res)
                | _ -> parse_var()
        in
        parse_app left

    and parse_abs() =
        eat '\\';
        let name = parse_name "" in
        eat '.';
        Abs (name, parse_lambda())

    and parse_app left =
        match (get()) with
            | ';' | ')' -> left
            | _ ->
                (eat ' ';
                 match (get()) with
                       | '\\' ->
                           (let right = parse_abs() in
                            parse_app (App (left, right)))
                       | '(' ->
                           (eat '(';
                            let right = parse_lambda() in
                            eat ')';
                            parse_app (App (left, right)))
                       | _ ->
                           (let right = parse_var() in
                            parse_app (App (left, right))))
    in

    parse_lambda();;
