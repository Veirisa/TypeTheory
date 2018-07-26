module Main where

import           AlgebraicTerm hiding (AlgebraicTerm)
import           Lambda        hiding (Lambda)
import           Reduction
import           Unify         hiding (AlgebraicTerm)

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

systemToSolutionString :: [String] -> [String] -> (String, String)
systemToSolutionString s1 s2 =
  let
    solution = systemToSolution $ zip (map algTermOfString s1) (map algTermOfString s2)
  in
    (stringOfAlgTerm $ fst solution, stringOfAlgTerm $ snd solution)
