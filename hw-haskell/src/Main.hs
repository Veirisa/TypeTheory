module Main where

import           Hw

main :: IO ()
main = putStrLn "TypeTheory"

--------------------------------- HW1 (Parser) ---------------------------------

doubleConvert :: String -> String
doubleConvert = stringOfLambda . lambdaOfString
