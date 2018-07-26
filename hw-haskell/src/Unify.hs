module Unify where

import           AlgebraicTerm

import           Data.List     (maximum)
import qualified Data.Map      as M (Map, empty, fromList, insert, lookup,
                                     toList)

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
    getPairMaxLength (left, right) = max (getMaxLength left) (getMaxLength right)

    getMaxLength :: AlgebraicTerm -> Int
    getMaxLength (Fun n l') = max (length n) (maximum $ map getMaxLength l')
    getMaxLength (Var x)    = length x

--------------------------------------------------------------------------------

-- Применение подстановки к уравнению
applySubstitution :: [(String, AlgebraicTerm)] -> AlgebraicTerm -> AlgebraicTerm
applySubstitution l = substitution (M.fromList l)
  where
    substitution :: M.Map String AlgebraicTerm -> AlgebraicTerm -> AlgebraicTerm
    substitution m (Fun n l) = Fun n (map (substitution m) l)
    substitution m at@(Var x) =
        case M.lookup x m of
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

--------------------------------------------------------------------------------

-- Решение системы (Nothing - если его не существует)
solveSystem :: [(AlgebraicTerm, AlgebraicTerm)] -> Maybe [(String, AlgebraicTerm)]
solveSystem lEq =
  let
    (left, right) = systemToEquation lEq
  in
    fmap M.toList (solve (Just M.empty) left right)
  where
    solve :: Maybe (M.Map String AlgebraicTerm) -> AlgebraicTerm -> AlgebraicTerm
             -> Maybe (M.Map String AlgebraicTerm)
    solve mm (Var x) at@(Fun _ _) = fmap (M.insert x at) mm
    solve mm at@(Fun _ _) (Var x) = fmap (M.insert x at) mm
    solve mm (Fun n1 l1) (Fun n2 l2) =
        if n1 == n2
        then undefined
        else Nothing
