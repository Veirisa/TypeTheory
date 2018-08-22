module Main where

import           Types.AlgebraicTerm
import           Types.HMLambda
import           Types.HMType
import           Types.Lambda
import           Types.SimpType

import           Inference
import           Reduction
import           Unify

main :: IO ()
main = putStrLn "TypeTheory"

--------------------------------- HW: Reduction --------------------------------

doubleConvertLambda :: String -> String
doubleConvertLambda = stringOfLambda . lambdaOfString


freeVarsString :: String -> [String]
freeVarsString = freeVars . lambdaOfString

freeToSubstString :: String -> String -> String -> Bool
freeToSubstString slS lS =
    freeToSubst (lambdaOfString slS) (lambdaOfString lS)

isNormalFormString :: String -> Bool
isNormalFormString = isNormalForm . lambdaOfString

isAlphaEquivalentString :: String -> String -> Bool
isAlphaEquivalentString s1 s2 =
    isAlphaEquivalent (lambdaOfString s1) (lambdaOfString s2)

normalBetaReductionString :: String -> String
normalBetaReductionString = stringOfLambda . normalBetaReduction . lambdaOfString

{-

"(\\x.x) a"                -> "a"
"a ((\\y.\\z.y) (\\p.p))"  -> "(a (\\z.(\\p.p)))"
"(\\x.x) (\\y.y) (\\z.z))" -> "((\\y.y) (\\z.z))"
"\\z.((\\x.x) y)"          -> "(\\z.y)"
"((\\x.\\y.x)(\\z.y)) k"   -> "((\\y0.(\\z.y)) k)"

-}

slowReduceToNormalFormString :: String -> String
slowReduceToNormalFormString = stringOfLambda . slowReduceToNormalForm . lambdaOfString

reduceToNormalFormString :: String -> String
reduceToNormalFormString = stringOfLambda . reduceToNormalForm . lambdaOfString

{-

"(\\x.\\y.y) ((\\z.z z) (\\z.z z))"
reduce to:
"(\\y0.y0)"

"a ((\\y.\\z.y) (\\p.p))"
reduce to:
"(a (\\z.(\\p1.p1)))"

"(\\x.x) (\\y.y) (\\z.z))"
reduce to:
"(\\z1.z1)"

"(\\x.x x) (\\a.\\b.b b b)"
reduce to:
"(\\b1.((b1 b1) b1))"

"(\\x.x x x) ((\\x.x) (\\x.x))"
reduce to:
"(\\x8.x8)"

"(\\x.\\y.x) (\\x.x) ((\\x.x x) (\\x.x x))"
reduce to:
"(\\x4.x4)"

"(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\f.\\x.f (f (f x)))"
reduce to:
"(\\f0.(\\x0.(f0 (f0 x0))))"

"((\\x.\\y.x)(\\z.y)) k"
reduce to:
"(\\z0.y)"

"(\\x.\\y.x) k"
reduce to:
"(\\y.k)"

"(\\y.\\m.y (\\f.\\n.(\\s.(s (\\x.\\a.\\b.b) (\\a.\\b.a)) (\\f.\\x.x) (f s)) (m n)) (\\f.\\x.f (f (f x)))) (\\f.(\\x.f (x x)) (\\x.f (x x))) ((\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)))"
reduce to:
"(\\f33.(\\x44.x44))"

"(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\f.\\x.f (f (f x)))"
reduce to:
"(\\f0.(\\x0.(f0 (f0 x0))))"

"((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\l13.((l13 (\\l14.(\\l15.(l14 (l14 l15))))) (\\l14.(\\l15.(l14 (l14 (l14 l15))))))) (\\l13.(\\l14.(((l0 (\\l15.(\\l16.(\\l17.(((l1 (l10 l16)) (l12 l17)) (((l1 (l10 l17)) ((l15 (l11 l16)) (\\l18.(\\l19.(l18 l19))))) ((l15 (l11 l16)) ((l15 l16) (l11 l17))))))))) l13) l14))))) (\\l12.(\\l13.(\\l14.((l12 l13) (l13 l14))))))) (\\l11.(\\l12.(\\l13.(((l11 (\\l14.(\\l15.(l15 (l14 l12))))) (\\l14.l13)) (\\l14.l14))))))) (\\l10.((l10 (\\l11.l3)) l2)))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\l13.(((l1 l12) l13) (((l1 l13) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))"
reduce to:
"(\\l5027.(\\l5028.(l5027 (l5027 (l5027 (l5027 (l5027 (l5027 (l5027 (l5027 (l5027 l5028)))))))))))"

"(\\s.\\k.\\i.(((s ((s (k s)) ((s ((s (k s)) ((s (k k)) i))) (k ((s (k (s ((s (k s)) ((s (k (s (k (s ((s ((s ((s i) (k (k (k i))))) (k ((s (k k)) i)))) (k ((s ((s (k s)) ((s (k k)) i))) (k i))))))))) ((s ((s (k s)) ((s (k k)) ((s (k s)) ((s (k (s (k ((s ((s (k s)) ((s (k k)) ((s (k s)) ((s (k k)) i))))) (k ((s ((s (k s)) ((s (k k)) i))) (k i)))))))) ((s ((s (k s)) ((s (k k)) i))) (k i))))))) (k ((s (k k)) i)))))))) ((s (k k)) ((s ((s (k s)) ((s (k k)) i))) (k i)))))))) (k (k ((s ((s (k s)) ((s (k k)) i))) ((s ((s (k s)) ((s (k k)) i))) ((s ((s (k s)) ((s (k k)) i))) (k i))))))) ((s ((s ((s (k s)) ((s (k k)) i))) (k ((s i) i)))) ((s ((s (k s)) ((s (k k)) i))) (k ((s i) i))))) ((s ((s (k s)) ((s (k (s (k s)))) ((s ((s (k s)) ((s (k (s (k s)))) ((s (k (s (k k)))) ((s ((s (k s)) ((s (k k)) i))) (k ((s (k (s (k (s i))))) ((s (k (s (k k)))) ((s (k (s i))) ((s (k k)) i)))))))))) (k (k ((s (k k)) i))))))) (k (k (k i))))) (\\x.\\y.\\z.x z (y z)) (\\x.\\y.x) (\\x.x)"
reduce to:
"(\\z872.(\\z873.(z872 (z872 (z872 (z872 (z872 (z872 z873))))))))"

-}

----------------------------------- HW: Unify ----------------------------------

doubleConvertAlgTerm :: String -> String
doubleConvertAlgTerm = stringOfAlgTerm . algTermOfString


systemToEquationString :: [String] -> [String] -> (String, String)
systemToEquationString ls1 ls2 =
  let
    result = systemToEquation $ zip (map algTermOfString ls1) (map algTermOfString ls2)
  in
    (stringOfAlgTerm $ fst result, stringOfAlgTerm $ snd result)

applySubstitutionString :: [(String, String)] -> String -> String
applySubstitutionString sSub s =
    stringOfAlgTerm $ applySubstitution (map (fmap algTermOfString) sSub) (algTermOfString s)

checkSolutionString :: [(String, String)] -> [String] -> [String] -> Bool
checkSolutionString lsSub ls1 ls2 =
  let
    lSub = map (fmap algTermOfString) lsSub
    lEq = zip (map algTermOfString ls1) (map algTermOfString ls2)
  in
    checkSolution lSub lEq

solveSystemString :: [(String, String)] -> Maybe [(String, String)]
solveSystemString lEq =
    case solveSystem $ map (\(s1, s2) -> (algTermOfString s1, algTermOfString s2)) lEq of
        Just lRes -> Just $ map (fmap stringOfAlgTerm) lRes
        _         -> Nothing

{-

x = b -> a -> b         : f b (f a b)
x = (y -> y) -> z       : f (f y yg) z

[("(f b (f a b))", "(f (f y y) z)")]
solution:
[("b","(f y y)"),
 ("z","(f a (f y y))")]

[("x","(f (f y y) z)"),
 ("x", "(f b (f a b))")]
solution:
[("b","(f y y)")
 ("z","(f a (f y y))")
 ("x","(f (f y y) (f a (f y y)))")]


a = b -> y              : f b y
a = y -> g              : f y g

[("a", "(f b y)"),
 ("a", "(f y g)")]
solution:
[("y","g"),
 ("b","g"),
 ("a","(f g g)")]

-}

--------------------------------- HW: Inference --------------------------------

doubleConvertHMLambda :: String -> String
doubleConvertHMLambda = stringOfHMLambda . hmLambdaOfString


inferSimpTypeString :: String -> Maybe ([(String, String)], String)
inferSimpTypeString sl =
    case inferSimpType $ lambdaOfString sl of
        Just (lRes, st) -> Just (map (fmap stringOfSimpType) lRes, stringOfSimpType st)
        _ -> Nothing

{-

"\\x.x"
substitution:
type:
"(t0 -> t0)"

"(\\f.(\\x.(f x)))"
substitution:
[("t0",
  "(t1 -> t3)")]
type:
"((t1 -> t3) -> (t1 -> t3))"

"(\\f.(\\x.(f (f x))))"
substitution:
[("t8","t1"),
 ("t3","t1"),
 ("t0","(t1 -> t1)")]
type:
"((t1 -> t1) -> (t1 -> t1))"

-}

algorithmWString :: String -> Maybe ([(String, String)], String)
algorithmWString sl =
    case algorithmW $ hmLambdaOfString sl of
        Just (lRes, st) -> Just (map (fmap stringOfHMType) lRes, stringOfHMType st)
        _ -> Nothing

{-

"\\x.x"
substitution:
type:
"(t0 -> t0)"

"(\\f.(\\x.(f x)))"
substitution:
[("t0",
  "(t1 -> t2)")]
type:
"((t1 -> t2) -> (t1 -> t2))"

"(\\f.(\\x.(f (f x))))"
substitution:
[("t0","(t3 -> t3)"),
 ("t1","t3"),
 ("t2","t3")]
type:
"((t3 -> t3) -> (t3 -> t3))"

"(let d = (\\x.x) in (\\f.(\\x.(d (d (d x))))))"
substitution:
([("t2","t8"),
  ("t3","t8"),
  ("t4","t8"),
  ("t5","t8"),
  ("t6","t8"),
  ("t7","t8")]
type:
"(t1 -> (t8 -> t8))"

"(let d = (\\t.t) in (\\f.(\\x.((d f) (d x)))))"
substitution:
[("t1","(t6 -> t7)"),
 ("t2","t6"),
 ("t3","(t6 -> t7)"),
 ("t4","(t6 -> t7)"),
 ("t5","t6")]
type:
"((t6 -> t7) -> (t6 -> t7))"

"(let d = (\\t.t) in (\\f.(\\x.((d f) ((d f) (d x))))))"
substitution:
[("t1","t4"),
 ("t2","t10"),
 ("t3","t4"),
 ("t4","(t10 -> t10)"),
 ("t5","(t10 -> t10)"),
 ("t6","(t10 -> t10)"),
 ("t7","t10"),
 ("t8","t10"),
 ("t9","t10")]
type:
"(t4 -> (t10 -> t10))"

"let w = (\\f.(\\x.(f (f x)))) in ((w (w (w (w (w w))))))"
substitution:
[("t0","(t3 -> t3)"),
 ("t1","t3"),
 ("t10","((t9 -> t9) -> (t9 -> t9))"),
 ("t11","((t9 -> t9) -> (t9 -> t9))"),
 ("t12","((t9 -> t9) -> (t9 -> t9))"),
 ("t13","((t9 -> t9) -> (t9 -> t9))"),
 ("t14","((t9 -> t9) -> (t9 -> t9))"),
 ("t2","t3"),
 ("t4","(t9 -> t9)"),
 ("t5","(t9 -> t9)"),
 ("t6","(t9 -> t9)"),
 ("t7","(t9 -> t9)"),
 ("t8","(t9 -> t9)")]
type:
"((t9 -> t9) -> (t9 -> t9))"

"(let a = (\\f.(\\x.(f (f x)))) in (let b = (\\f.(\\x.(f (f x)))) in (let c = (\\f.(\\x.(f (f x)))) in ((a (b c))))))"
substitution:
[("t0","(t3 -> t3)"),
 ("t1","t3"),
 ("t10","t11"),
 ("t12","(t14 -> t14)"),
 ("t13","(t14 -> t14)"),
 ("t15","((t14 -> t14) -> (t14 -> t14))"),
 ("t16","((t14 -> t14) -> (t14 -> t14))"),
 ("t2","t3"),
 ("t4","(t7 -> t7)"),
 ("t5","t7"),
 ("t6","t7"),
 ("t8","(t11 -> t11)"),
 ("t9","t11")]
type:
"((t14 -> t14) -> (t14 -> t14))"

-}
