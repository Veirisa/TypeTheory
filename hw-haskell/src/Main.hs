module Main where

import           AlgebraicTerm
import           Inference
import           Lambda
import           Reduction
import           SimpType
import           Unify

import qualified Data.Set      as S (Set)

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
ans = Just [("b","(f y y)"),("z","(f a (f y y))")]
[("x", "(f (f y y) z)"), ("x", "(f b (f a b))")]
ans = Just [("b","(f y y)"),("z","(f a (f y y))"),("x","(f (f y y) (f a (f y y)))")]
-}

{-
a = b -> y              : f b y
a = y -> g              : f y g

[("a", "(f b y)"), ("a", "(f y g)")]
ans = Just [("y","g"),("b","g"),("a","(f g g)")]
-}

--------------------------------- HW: Inference --------------------------------

-- inferSimpType :: Lambda -> Maybe ([(String, SimpType)], SimpType)

inferSimpTypeString :: String -> Maybe ([(String, String)], String)
inferSimpTypeString sl =
    case inferSimpType $ lambdaOfString sl of
        Just (lRes, st) -> Just (map (fmap stringOfSimpType) lRes, stringOfSimpType st)
        _ -> Nothing

-- "(\\f.(\\x.(f (f x))))"
