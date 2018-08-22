module Main where

import           AlgebraicTerm
import           HMLambda
import           HMType
import           Inference
import           Lambda
import           Reduction
import           SimpType
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

slowReduceToNormalFormString :: String -> String
slowReduceToNormalFormString = stringOfLambda . slowReduceToNormalForm . lambdaOfString

reduceToNormalFormString :: String -> String
reduceToNormalFormString = stringOfLambda . reduceToNormalForm . lambdaOfString

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

-}

{-

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
