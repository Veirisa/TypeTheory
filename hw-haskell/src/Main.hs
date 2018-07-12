module Main where

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

freeToSubstFromStrings :: String -> String -> String -> Bool
freeToSubstFromStrings slS lS =
    freeToSubst (lambdaOfString slS) (lambdaOfString lS)

isNormalFormFromString :: String -> Bool
isNormalFormFromString = isNormalForm . lambdaOfString

isAlphaEqFromStrings :: String -> String -> Bool
isAlphaEqFromStrings s1 s2 =
    isAlphaEquivalent (lambdaOfString s1) (lambdaOfString s2)

normalBetaReductionStrings :: String -> String
normalBetaReductionStrings = stringOfLambda . normalBetaReduction . lambdaOfString

substitutionStrings :: String -> String -> String -> String
substitutionStrings s1 s2 s =
     stringOfLambda $ substitution (lambdaOfString s1) (lambdaOfString s2) s
