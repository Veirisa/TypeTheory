module Unify where

import           AlgebraicTerm

import           Data.List     (maximum)
import qualified Data.Map      as M (Map, fromList, lookup)

--------------------------------------------------------------------------------

-- Одно уравнение по списку уравнений
systemToEquation :: [(AlgebraicTerm, AlgebraicTerm)] -> (AlgebraicTerm, AlgebraicTerm)
systemToEquation l =
  let
    name = uniqueName l
  in
    (Fun name (map fst l), Fun name (map snd l))
  where
    uniqueName :: [(AlgebraicTerm, AlgebraicTerm)] -> String
    uniqueName l' = replicate (maximum (map getPairMaxLength l') + 1) 'f'

    getPairMaxLength :: (AlgebraicTerm, AlgebraicTerm) -> Int
    getPairMaxLength (x, y) = max (getMaxLength x) (getMaxLength y)

    getMaxLength :: AlgebraicTerm -> Int
    getMaxLength (Fun n l') = max (length n) (maximum $ map getMaxLength l')
    getMaxLength (Var n)    = length n

--------------------------------------------------------------------------------

-- Применение подстановки к уравнению
applySubstitution :: [(String, AlgebraicTerm)] -> AlgebraicTerm -> AlgebraicTerm
applySubstitution l = substitution (M.fromList l)
  where
    substitution :: M.Map String AlgebraicTerm -> AlgebraicTerm -> AlgebraicTerm
    substitution m (Fun n l) = Fun n (map (substitution m) l)
    substitution m at@(Var n) =
        case M.lookup n m of
            Just newAt -> newAt
            Nothing    -> at

--------------------------------------------------------------------------------

-- Проверка решения
checkSolution :: [(String, AlgebraicTerm)] -> [(AlgebraicTerm, AlgebraicTerm)] -> Bool
checkSolution lSub lEq =
  let
    (left, right) = systemToEquation lEq
  in
    applySubstitution lSub left == applySubstitution lSub right
