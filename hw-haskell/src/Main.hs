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
