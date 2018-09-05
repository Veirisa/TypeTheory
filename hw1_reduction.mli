open Hw1

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
val free_to_subst: lambda -> lambda -> string -> bool

(* Вернуть список имён свободных переменных *)
val free_vars: lambda -> string list

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
val is_normal_form: lambda -> bool

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
val is_alpha_equivalent: lambda -> lambda -> bool

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
val normal_beta_reduction: lambda -> lambda

val slow_reduce_to_normal_form: lambda -> lambda

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать
   мемоизацию *)
val reduce_to_normal_form: lambda -> lambda
