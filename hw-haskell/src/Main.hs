module Main where

import qualified Data.Set    as S (Set)
import           Hw
import           HwReduction

main :: IO ()
main = putStrLn "TypeTheory"

--------------------------------- HW1 (Parser) ---------------------------------

doubleConvert :: String -> String
doubleConvert = stringOfLambda . lambdaOfString

-------------------------------- HW1: Reduction --------------------------------

freeVarsFromString :: String -> [String]
freeVarsFromString = freeVars . lambdaOfString

failsFreeToSubstFromStrings :: String -> String -> String -> S.Set String
failsFreeToSubstFromStrings slS lS =
    failsFreeToSubst (lambdaOfString slS) (lambdaOfString lS)

isNormalFormFromString :: String -> Bool
isNormalFormFromString = isNormalForm . lambdaOfString

isAlphaEqFromStrings :: String -> String -> Bool
isAlphaEqFromStrings s1 s2 =
    isAlphaEquivalent (lambdaOfString s1) (lambdaOfString s2)

normalBetaReductionStrings :: String -> String
normalBetaReductionStrings = stringOfLambda . normalBetaReduction . lambdaOfString
